module git_ops
    use iso_fortran_env, only: output_unit
    use terminal_control, only: GREEN, RED, GREY, RESET, BOLD, CLEAR
    use filesystem_ops, only: MAX_PATH
    implicit none
    private

    public :: detect_git_repo, get_git_status, write_git_indicators
    public :: git_add_file, git_unstage_file, git_commit_prompt
    public :: git_push_prompt, git_tag_prompt, prompt_upstream_selection
    public :: show_git_diff_fullscreen

contains

    subroutine detect_git_repo(dir, is_git, repo, branch)
        character(len=*), intent(in) :: dir
        logical, intent(out) :: is_git
        character(len=*), intent(out) :: repo, branch
        integer :: stat, i
        character(len=MAX_PATH) :: temp_file

        is_git = .false.
        repo = ""
        branch = ""

        ! Check if .git directory exists
        call execute_command_line("git -C '" // trim(dir) // "' rev-parse --git-dir > /dev/null 2>&1", &
                                  exitstat=stat, wait=.true.)
        is_git = (stat == 0)

        if (is_git) then
            ! Get repo name (basename of repo root)
            call get_environment_variable("HOME", temp_file)
            temp_file = trim(temp_file) // "/.fortress_repo"
            call execute_command_line("git -C '" // trim(dir) // "' rev-parse --show-toplevel 2>/dev/null | " // &
                                     "xargs basename > " // trim(temp_file), wait=.true.)
            open(newunit=stat, file=temp_file, status='old', iostat=i)
            if (i == 0) then
                read(stat, '(a)', iostat=i) repo
                close(stat)
            end if
            call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")

            ! Get current branch name
            temp_file = trim(temp_file) // "_branch"
            call execute_command_line("git -C '" // trim(dir) // "' rev-parse --abbrev-ref HEAD 2>/dev/null > " // &
                                     trim(temp_file), wait=.true.)
            open(newunit=stat, file=temp_file, status='old', iostat=i)
            if (i == 0) then
                read(stat, '(a)', iostat=i) branch
                close(stat)
            end if
            call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
        end if
    end subroutine detect_git_repo

    subroutine get_git_status(dir, files, count, is_staged, is_unstaged, is_untracked)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        logical, dimension(*), intent(out) :: is_staged, is_unstaged, is_untracked
        character(len=MAX_PATH) :: temp_file, line, file_path, git_status
        integer :: unit, ios, stat, i

        ! Initialize all to false
        do i = 1, count
            is_staged(i) = .false.
            is_unstaged(i) = .false.
            is_untracked(i) = .false.
        end do

        ! Get git status
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_git_status"
        call execute_command_line("cd '" // trim(dir) // "' && git status --porcelain 2>/dev/null > " // &
                                 trim(temp_file), exitstat=stat, wait=.true.)

        if (stat /= 0) return

        ! Parse git status output
        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios /= 0) return

        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit

            if (len_trim(line) > 3) then
                git_status = line(1:2)
                file_path = trim(adjustl(line(4:)))

                ! Match against our file list
                do i = 1, count
                    if (trim(files(i)) == trim(file_path)) then
                        ! Parse git status (XY format)
                        is_untracked(i) = (git_status == '??')
                        is_staged(i) = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                        is_unstaged(i) = (git_status(2:2) /= ' ' .and. .not. is_untracked(i))
                        exit
                    end if
                end do
            end if
        end do

        close(unit)
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
    end subroutine get_git_status

    subroutine write_git_indicators(staged, unstaged, untracked, highlighted)
        logical, intent(in) :: staged, unstaged, untracked, highlighted

        ! Write indicators without RESET (caller handles that)
        if (staged) then
            if (highlighted) then
                write(output_unit, '(a)', advance='no') GREEN // " ↑"
            else
                write(output_unit, '(a)', advance='no') GREEN // " ↑" // RESET
            end if
        end if
        if (unstaged) then
            if (highlighted) then
                write(output_unit, '(a)', advance='no') RED // " ✗"
            else
                write(output_unit, '(a)', advance='no') RED // " ✗" // RESET
            end if
        end if
        if (untracked) then
            if (highlighted) then
                write(output_unit, '(a)', advance='no') GREY // " ✗"
            else
                write(output_unit, '(a)', advance='no') GREY // " ✗" // RESET
            end if
        end if
    end subroutine write_git_indicators

    subroutine git_add_file(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=MAX_PATH*2) :: git_cmd
        integer :: stat

        ! Build git add command
        git_cmd = "cd '" // trim(dir) // "' && git add '" // trim(filename) // "' 2>/dev/null"
        call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)
    end subroutine git_add_file

    subroutine git_unstage_file(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=MAX_PATH*2) :: git_cmd
        integer :: stat

        ! Build git restore --staged command
        git_cmd = "cd '" // trim(dir) // "' && git restore --staged '" // trim(filename) // "' 2>/dev/null"
        call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)
    end subroutine git_unstage_file

    subroutine git_commit_prompt(dir, repo_name)
        character(len=*), intent(in) :: dir, repo_name
        character(len=512) :: commit_msg
        character(len=MAX_PATH*2) :: git_cmd
        character(len=1) :: key
        integer :: stat, ios

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)', advance='no') BOLD // "Git Commit" // RESET // " - " // trim(repo_name)
        write(output_unit, *)
        write(output_unit, *)
        write(output_unit, '(a)', advance='no') "Commit message: "

        ! Restore terminal to canonical mode for reading input
        call execute_command_line("stty icanon echo 2>/dev/null")

        ! Read commit message
        read(*, '(a)', iostat=ios) commit_msg

        ! Restore raw mode
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")

        if (ios == 0 .and. len_trim(commit_msg) > 0) then
            ! Execute git commit (use single quotes for message to avoid escaping issues)
            git_cmd = "cd '" // trim(dir) // "' && git commit -m '" // trim(commit_msg) // "' 2>&1"
            call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)

            ! Show result briefly
            write(output_unit, *)
            if (stat == 0) then
                write(output_unit, '(a)') GREEN // "✓ Committed successfully!" // RESET
            else
                write(output_unit, '(a)') RED // "✗ Commit failed (nothing to commit?)" // RESET
            end if
            write(output_unit, '(a)') "Press any key to continue..."

            ! Wait for keypress
            read(*, '(a1)', advance='no') key
        end if
    end subroutine git_commit_prompt

    subroutine prompt_upstream_selection(dir, success)
        character(len=*), intent(in) :: dir
        logical, intent(out) :: success
        character(len=MAX_PATH) :: temp_file, selected_branch
        integer :: stat, unit, ios

        success = .false.

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)') BOLD // "No upstream branch configured" // RESET
        write(output_unit, *)
        write(output_unit, '(a)') "Select a remote branch to track:"
        write(output_unit, *)

        ! Restore terminal for fzf
        call execute_command_line("stty sane 2>/dev/null")

        ! Use fzf to select remote branch
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_upstream"
        call execute_command_line("cd '" // trim(dir) // "' && git branch -r | grep -v HEAD | " // &
                                 "sed 's/^[[:space:]]*//' | fzf --height=10 --prompt='Select upstream: ' > " // &
                                 trim(temp_file) // " 2>/dev/null", exitstat=stat, wait=.true.)

        if (stat /= 0) then
            write(output_unit, '(a)') RED // "No upstream selected." // RESET
            call execute_command_line("sleep 1")
            ! Re-enable raw mode
            call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")
            call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
            return
        end if

        ! Read selected branch
        open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)', iostat=ios) selected_branch
            close(unit)

            if (ios == 0 .and. len_trim(selected_branch) > 0) then
                ! Set upstream
                call execute_command_line("cd '" // trim(dir) // "' && git branch --set-upstream-to=" // &
                                         trim(selected_branch) // " 2>&1", exitstat=stat, wait=.true.)

                if (stat == 0) then
                    write(output_unit, '(a)') GREEN // "✓ Upstream set to: " // trim(selected_branch) // RESET
                    success = .true.
                else
                    write(output_unit, '(a)') RED // "✗ Failed to set upstream" // RESET
                end if
                call execute_command_line("sleep 1")
            end if
        end if

        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
        ! Re-enable raw mode
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")
    end subroutine prompt_upstream_selection

    subroutine git_push_prompt(dir, repo_name)
        character(len=*), intent(in) :: dir, repo_name
        character(len=MAX_PATH*2) :: git_cmd
        character(len=1) :: key
        integer :: stat
        logical :: upstream_set

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)', advance='no') BOLD // "Git Push" // RESET // " - " // trim(repo_name)
        write(output_unit, *)
        write(output_unit, *)

        ! Check if upstream is configured
        call execute_command_line("cd '" // trim(dir) // "' && git rev-parse --abbrev-ref @{upstream} " // &
                                 "> /dev/null 2>&1", exitstat=stat, wait=.true.)

        if (stat /= 0) then
            ! No upstream configured - prompt user to select one
            call prompt_upstream_selection(dir, upstream_set)
            if (.not. upstream_set) return
            ! Clear screen again after upstream selection
            write(output_unit, '(a)', advance='no') CLEAR
            write(output_unit, '(a)', advance='no') BOLD // "Git Push" // RESET // " - " // trim(repo_name)
            write(output_unit, *)
            write(output_unit, *)
        end if

        write(output_unit, '(a)') "Pushing to remote..."
        write(output_unit, *)

        ! Execute git push
        git_cmd = "cd '" // trim(dir) // "' && git push 2>&1"
        call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)

        ! Show result
        write(output_unit, *)
        if (stat == 0) then
            write(output_unit, '(a)') GREEN // "✓ Pushed successfully!" // RESET
        else
            write(output_unit, '(a)') RED // "✗ Push failed" // RESET
        end if
        write(output_unit, '(a)') "Press any key to continue..."

        ! Wait for keypress
        read(*, '(a1)', advance='no') key
    end subroutine git_push_prompt

    subroutine git_tag_prompt(dir, repo_name)
        character(len=*), intent(in) :: dir, repo_name
        character(len=512) :: tag_name, tag_message
        character(len=MAX_PATH*2) :: git_cmd
        character(len=1) :: key
        integer :: stat, ios

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)', advance='no') BOLD // "Git Tag" // RESET // " - " // trim(repo_name)
        write(output_unit, *)
        write(output_unit, *)
        write(output_unit, '(a)', advance='no') "Tag name: "

        ! Restore terminal to canonical mode for reading input
        call execute_command_line("stty icanon echo 2>/dev/null")

        ! Read tag name
        read(*, '(a)', iostat=ios) tag_name

        if (ios == 0 .and. len_trim(tag_name) > 0) then
            ! Read tag message (optional)
            write(output_unit, '(a)', advance='no') "Tag message (enter for none): "
            read(*, '(a)', iostat=ios) tag_message

            ! Restore raw mode
            call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")

            if (ios == 0) then
                ! Execute git tag
                if (len_trim(tag_message) > 0) then
                    ! Create annotated tag with message
                    git_cmd = "cd '" // trim(dir) // "' && git tag -a '" // trim(tag_name) // &
                             "' -m '" // trim(tag_message) // "' 2>&1"
                else
                    ! Create lightweight tag (no message)
                    git_cmd = "cd '" // trim(dir) // "' && git tag '" // trim(tag_name) // "' 2>&1"
                end if

                call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)

                ! Show result
                write(output_unit, *)
                if (stat == 0) then
                    write(output_unit, '(a)') GREEN // "✓ Tag created: " // trim(tag_name) // RESET
                else
                    write(output_unit, '(a)') RED // "✗ Failed to create tag" // RESET
                end if
                write(output_unit, '(a)') "Press any key to continue..."

                ! Wait for keypress
                read(*, '(a1)', advance='no') key
            end if
        else
            ! Restore raw mode if tag name was empty
            call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")
        end if
    end subroutine git_tag_prompt

    subroutine show_git_diff_fullscreen(dir, filename, is_staged, is_unstaged)
        character(len=*), intent(in) :: dir, filename
        logical, intent(in) :: is_staged, is_unstaged
        character(len=MAX_PATH*2) :: git_cmd
        character(len=1) :: key

        ! Clear screen
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)') BOLD // "Git Diff" // RESET // " - " // trim(filename)
        write(output_unit, *)

        ! Restore terminal to normal mode for better output
        call execute_command_line("stty sane 2>/dev/null")

        ! Build and execute diff command (show both staged and unstaged if both exist)
        if (is_unstaged) then
            write(output_unit, '(a)') BOLD // "Unstaged changes:" // RESET
            git_cmd = "cd '" // trim(dir) // "' && git diff --color=always '" // trim(filename) // "' 2>&1"
            call execute_command_line(trim(git_cmd), wait=.true.)
            write(output_unit, *)
        end if

        if (is_staged) then
            write(output_unit, '(a)') BOLD // "Staged changes:" // RESET
            git_cmd = "cd '" // trim(dir) // "' && git diff --cached --color=always '" // trim(filename) // "' 2>&1"
            call execute_command_line(trim(git_cmd), wait=.true.)
            write(output_unit, *)
        end if

        ! Wait for keypress
        write(output_unit, *)
        write(output_unit, '(a)') GREY // "Press any key to return..." // RESET

        ! Restore raw mode before reading
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")
        read(*, '(a1)', advance='no') key
    end subroutine show_git_diff_fullscreen

end module git_ops
