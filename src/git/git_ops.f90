module git_ops
    use iso_fortran_env, only: output_unit
    use terminal_control, only: GREEN, RED, GREY, YELLOW, RESET, BOLD, CLEAR
    use filesystem_ops, only: MAX_PATH, MAX_FILES
    implicit none
    private

    public :: detect_git_repo, get_git_status, write_git_indicators
    public :: git_add_file, git_unstage_file, git_commit_prompt
    public :: git_push_prompt, git_tag_prompt, prompt_upstream_selection
    public :: show_git_diff_fullscreen
    public :: invalidate_git_cache
    public :: git_fetch_prompt, git_pull_prompt
    public :: mark_incoming_changes

    ! Cache variables for git status
    logical :: cache_valid = .false.
    character(len=MAX_PATH) :: cached_dir = ""
    integer :: cache_timestamp = 0
    integer :: cache_count = 0
    character(len=MAX_PATH), dimension(MAX_FILES) :: cached_files
    logical, dimension(MAX_FILES) :: cached_staged, cached_unstaged, cached_untracked
    integer, parameter :: CACHE_TTL_MS = 500  ! Cache for 500ms

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

    subroutine invalidate_git_cache()
        cache_valid = .false.
    end subroutine invalidate_git_cache

    integer function get_time_ms()
        integer :: values(8)
        call date_and_time(values=values)
        ! Convert to milliseconds (rough approximation, good enough for cache)
        get_time_ms = values(5)*3600000 + values(6)*60000 + values(7)*1000 + values(8)
    end function get_time_ms

    logical function is_cache_valid(dir, files, count)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: current_time, i
        logical :: files_match

        is_cache_valid = .false.

        ! Check if cache exists and directory matches
        if (.not. cache_valid .or. trim(cached_dir) /= trim(dir)) return

        ! Check if cache is still fresh (within TTL)
        current_time = get_time_ms()
        if (abs(current_time - cache_timestamp) > CACHE_TTL_MS) return

        ! Check if file list matches (quick count check first)
        if (cache_count /= count) return

        ! Verify files are the same
        files_match = .true.
        do i = 1, count
            if (trim(cached_files(i)) /= trim(files(i))) then
                files_match = .false.
                exit
            end if
        end do

        is_cache_valid = files_match
    end function is_cache_valid

    subroutine get_git_status(dir, files, is_dir_arr, count, is_staged, is_unstaged, is_untracked)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir_arr
        integer, intent(in) :: count
        logical, dimension(*), intent(out) :: is_staged, is_unstaged, is_untracked
        character(len=MAX_PATH) :: temp_file, line, file_path, git_status
        character(len=MAX_PATH) :: repo_root, rel_path, full_status_path
        integer :: unit, ios, stat, i, repo_root_len

        ! Check if we can use cached data
        if (is_cache_valid(dir, files, count)) then
            ! Use cached results
            do i = 1, count
                is_staged(i) = cached_staged(i)
                is_unstaged(i) = cached_unstaged(i)
                is_untracked(i) = cached_untracked(i)
            end do
            return
        end if

        ! Initialize all to false
        do i = 1, count
            is_staged(i) = .false.
            is_unstaged(i) = .false.
            is_untracked(i) = .false.
        end do

        ! Get repo root
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_repo_root"
        call execute_command_line("git -C '" // trim(dir) // "' rev-parse --show-toplevel 2>/dev/null > " // &
                                 trim(temp_file), exitstat=stat, wait=.true.)
        if (stat /= 0) return

        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)', iostat=ios) repo_root
            close(unit)
        end if
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
        if (ios /= 0) return

        ! Calculate relative path from repo root to current directory
        repo_root_len = len_trim(repo_root)
        if (trim(dir) == trim(repo_root)) then
            rel_path = ""
        else if (len_trim(dir) > repo_root_len .and. dir(1:repo_root_len) == trim(repo_root)) then
            ! Remove repo_root + "/" from dir
            rel_path = dir(repo_root_len+2:)  ! +2 to skip the "/"
        else
            rel_path = ""
        end if

        ! Get git status from repo root
        temp_file = trim(temp_file) // "_status"
        call execute_command_line("cd '" // trim(repo_root) // "' && git status --porcelain 2>/dev/null > " // &
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
                full_status_path = trim(adjustl(line(4:)))

                ! Check if this file is in our current directory or subdirectory
                do i = 1, count
                    ! Build expected path for this file
                    if (len_trim(rel_path) > 0) then
                        file_path = trim(rel_path) // "/" // trim(files(i))
                    else
                        file_path = trim(files(i))
                    end if

                    ! For files: exact match
                    if (.not. is_dir_arr(i) .and. trim(full_status_path) == trim(file_path)) then
                        is_untracked(i) = (git_status == '??')
                        is_staged(i) = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                        is_unstaged(i) = (git_status(2:2) /= ' ' .and. .not. is_untracked(i))
                        exit
                    end if

                    ! For directories: check if status path starts with dirname/
                    if (is_dir_arr(i) .and. trim(files(i)) /= "." .and. trim(files(i)) /= "..") then
                        ! Build directory path
                        if (len_trim(rel_path) > 0) then
                            file_path = trim(rel_path) // "/" // trim(files(i)) // "/"
                        else
                            file_path = trim(files(i)) // "/"
                        end if

                        ! Check if status path starts with this directory
                        if (len_trim(full_status_path) >= len_trim(file_path) .and. &
                            full_status_path(1:len_trim(file_path)) == trim(file_path)) then
                            ! This directory contains dirty files
                            is_untracked(i) = is_untracked(i) .or. (git_status == '??')
                            is_staged(i) = is_staged(i) .or. (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                            is_unstaged(i) = is_unstaged(i) .or. (git_status(2:2) /= ' ' .and. git_status /= '??')
                        end if
                    end if
                end do
            end if
        end do

        close(unit)
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")

        ! Update cache
        cache_valid = .true.
        cached_dir = dir
        cache_timestamp = get_time_ms()
        cache_count = count
        do i = 1, count
            cached_files(i) = files(i)
            cached_staged(i) = is_staged(i)
            cached_unstaged(i) = is_unstaged(i)
            cached_untracked(i) = is_untracked(i)
        end do
    end subroutine get_git_status

    subroutine write_git_indicators(staged, unstaged, untracked, has_incoming, highlighted)
        logical, intent(in) :: staged, unstaged, untracked, has_incoming, highlighted

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
        if (has_incoming) then
            if (highlighted) then
                write(output_unit, '(a)', advance='no') YELLOW // " ↓"
            else
                write(output_unit, '(a)', advance='no') YELLOW // " ↓" // RESET
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

        ! Invalidate cache after git operation
        call invalidate_git_cache()
    end subroutine git_add_file

    subroutine git_unstage_file(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=MAX_PATH*2) :: git_cmd
        integer :: stat

        ! Build git restore --staged command
        git_cmd = "cd '" // trim(dir) // "' && git restore --staged '" // trim(filename) // "' 2>/dev/null"
        call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)

        ! Invalidate cache after git operation
        call invalidate_git_cache()
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
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null", wait=.true.)

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
            call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null", wait=.true.)
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
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null", wait=.true.)
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
            call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null", wait=.true.)

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
            call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null", wait=.true.)
        end if
    end subroutine git_tag_prompt

    subroutine show_git_diff_fullscreen(dir, filename, is_staged, is_unstaged)
        character(len=*), intent(in) :: dir, filename
        logical, intent(in) :: is_staged, is_unstaged
        character(len=MAX_PATH*2) :: git_cmd
        character(len=1) :: key
        integer :: ios

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

        ! Restore raw mode and give terminal time to settle
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null && sleep 0.05", wait=.true.)

        ! Read keypress with error handling for any buffering issues
        read(*, '(a1)', advance='no', iostat=ios) key
    end subroutine show_git_diff_fullscreen

    subroutine git_fetch_prompt(dir, repo_name)
        character(len=*), intent(in) :: dir, repo_name
        character(len=1) :: key
        integer :: stat
        logical :: upstream_set

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)', advance='no') BOLD // "Git Fetch" // RESET // " - " // trim(repo_name)
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
            write(output_unit, '(a)', advance='no') BOLD // "Git Fetch" // RESET // " - " // trim(repo_name)
            write(output_unit, *)
            write(output_unit, *)
        end if

        write(output_unit, '(a)') "Fetching from remote..."
        write(output_unit, *)

        ! Execute git fetch
        call execute_command_line("cd '" // trim(dir) // "' && git fetch 2>&1", exitstat=stat, wait=.true.)

        ! Show result
        write(output_unit, *)
        if (stat == 0) then
            write(output_unit, '(a)') GREEN // "✓ Fetch completed!" // RESET
        else
            write(output_unit, '(a)') RED // "✗ Fetch failed" // RESET
        end if
        write(output_unit, '(a)') "Press any key to continue..."

        ! Wait for keypress
        read(*, '(a1)', advance='no') key

        ! Invalidate git cache after fetch
        call invalidate_git_cache()
    end subroutine git_fetch_prompt

    subroutine git_pull_prompt(dir, repo_name)
        character(len=*), intent(in) :: dir, repo_name
        character(len=1) :: key
        integer :: stat
        logical :: upstream_set

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)', advance='no') BOLD // "Git Pull" // RESET // " - " // trim(repo_name)
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
            write(output_unit, '(a)', advance='no') BOLD // "Git Pull" // RESET // " - " // trim(repo_name)
            write(output_unit, *)
            write(output_unit, *)
        end if

        write(output_unit, '(a)') "Pulling from remote..."
        write(output_unit, *)

        ! Execute git pull
        call execute_command_line("cd '" // trim(dir) // "' && git pull 2>&1", exitstat=stat, wait=.true.)

        ! Show result
        write(output_unit, *)
        if (stat == 0) then
            write(output_unit, '(a)') GREEN // "✓ Pull completed!" // RESET
        else
            write(output_unit, '(a)') RED // "✗ Pull failed" // RESET
        end if
        write(output_unit, '(a)') "Press any key to continue..."

        ! Wait for keypress
        read(*, '(a1)', advance='no') key

        ! Invalidate git cache after pull
        call invalidate_git_cache()
    end subroutine git_pull_prompt

    subroutine mark_incoming_changes(dir, files, count, has_incoming)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        logical, dimension(*), intent(out) :: has_incoming
        character(len=MAX_PATH) :: temp_file, line, incoming_path
        integer :: unit, ios, stat, i

        ! Initialize all to false
        do i = 1, count
            has_incoming(i) = .false.
        end do

        ! Check if there's an upstream branch configured
        ! Don't prompt - this is called automatically during refresh
        call execute_command_line("cd '" // trim(dir) // "' && git rev-parse --abbrev-ref @{upstream} " // &
                                 "> /dev/null 2>&1", exitstat=stat, wait=.true.)
        if (stat /= 0) then
            ! No upstream configured - silently return
            return
        end if

        ! Get list of files that differ between HEAD and upstream
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_incoming"
        call execute_command_line("cd '" // trim(dir) // "' && " // &
                                 "git diff --name-only HEAD...@{upstream} > " // trim(temp_file) // " 2>/dev/null", &
                                 exitstat=stat, wait=.true.)

        if (stat /= 0) then
            ! If diff fails, no incoming changes
            call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
            return
        end if

        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios /= 0) then
            call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
            return
        end if

        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit

            if (len_trim(line) > 0) then
                incoming_path = trim(line)
                ! Mark this file as having incoming changes
                do i = 1, count
                    if (trim(files(i)) == trim(incoming_path)) then
                        has_incoming(i) = .true.
                        exit
                    end if
                end do
            end if
        end do

        close(unit)
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
    end subroutine mark_incoming_changes

end module git_ops
