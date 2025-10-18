program fortress_clean
    use iso_fortran_env, only: output_unit, error_unit
    implicit none

    ! Constants
    integer, parameter :: MAX_PATH = 512
    integer, parameter :: MAX_FILES = 500
    
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CLEAR = ESC // "[2J" // ESC // "[H"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: RED = ESC // "[31m"
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"

    ! Variables
    character(len=MAX_PATH) :: current_dir, parent_dir, temp_dir
    character(len=MAX_PATH), dimension(MAX_FILES) :: current_files, parent_files
    logical, dimension(MAX_FILES) :: current_is_dir, parent_is_dir
    logical, dimension(MAX_FILES) :: current_is_exec, parent_is_exec
    logical, dimension(MAX_FILES) :: current_is_staged, current_is_unstaged, current_is_untracked
    logical, dimension(MAX_FILES) :: parent_is_staged, parent_is_unstaged, parent_is_untracked
    integer :: current_count, parent_count
    integer :: selected = 1
    integer :: parent_selected = -1
    integer :: scroll_offset = 0
    integer :: parent_scroll_offset = 0
    character(len=1) :: key
    logical :: running = .true.
    logical :: cd_on_exit = .false.
    character(len=MAX_PATH) :: exit_dir
    character(len=256) :: repo_name
    logical :: in_git_repo = .false.
    integer :: i, rows, cols, visible_height

    ! Initialize
    current_dir = get_pwd()
    parent_dir = get_parent_path(current_dir)
    call detect_git_repo(current_dir, in_git_repo, repo_name)

    ! Setup terminal
    call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")

    ! Main loop
    do while (running)
        ! Get files
        call get_file_list(current_dir, current_files, current_is_dir, current_is_exec, current_count)
        call get_file_list(parent_dir, parent_files, parent_is_dir, parent_is_exec, parent_count)

        ! Initialize git arrays - only for actual file counts
        do i = 1, current_count
            current_is_staged(i) = .false.
            current_is_unstaged(i) = .false.
            current_is_untracked(i) = .false.
        end do
        do i = 1, parent_count
            parent_is_staged(i) = .false.
            parent_is_unstaged(i) = .false.
            parent_is_untracked(i) = .false.
        end do

        ! Get git status if in a repo
        if (in_git_repo) then
            call get_git_status(current_dir, current_files, current_count, &
                               current_is_staged, current_is_unstaged, current_is_untracked)
        end if

        ! Get terminal size early to use for scroll calculations
        call get_term_size(rows, cols)
        visible_height = rows - 3  ! Header + footer + 1 for indexing

        ! Handle navigation - find position in parent if needed
        if (selected == -1) then
            selected = find_in_parent(temp_dir, current_files, current_count)
            ! Center the cursor in viewport if possible
            scroll_offset = max(0, selected - visible_height / 2)
        else if (selected == -2) then
            ! Find position after fzf selection
            selected = find_file_in_list(temp_dir, current_files, current_count)
            ! Center the cursor in viewport
            scroll_offset = max(0, selected - visible_height / 2)
        end if

        ! Ensure selected cursor is within valid bounds
        if (current_count > 0) then
            selected = max(1, min(selected, current_count))
        else
            selected = 1
        end if

        ! Find current dir in parent
        parent_selected = find_in_parent(current_dir, parent_files, parent_count)

        ! Adjust scroll offset to keep selected item visible
        if (selected <= scroll_offset) then
            ! Scrolled above viewport - move viewport up
            scroll_offset = max(0, selected - 1)
        else if (selected > scroll_offset + visible_height) then
            ! Scrolled below viewport - move viewport down
            scroll_offset = selected - visible_height
        end if
        scroll_offset = max(0, min(scroll_offset, max(0, current_count - visible_height)))

        ! Adjust parent scroll offset to keep parent selection visible
        if (parent_selected > 0) then
            if (parent_selected <= parent_scroll_offset) then
                parent_scroll_offset = max(0, parent_selected - 1)
            else if (parent_selected > parent_scroll_offset + visible_height) then
                parent_scroll_offset = parent_selected - visible_height
            end if
            parent_scroll_offset = max(0, min(parent_scroll_offset, max(0, parent_count - visible_height)))
        end if

        ! Draw interface
        write(output_unit, '(a)', advance='no') CLEAR
        call draw_interface(rows, cols)

        ! Get input
        read(*, '(a1)', advance='no') key

        ! Handle input
        select case(ichar(key))
        case(27)  ! ESC sequence
            call read_arrow_key(key)
            select case(key)
            case('A')  ! Up
                if (selected > 1) then
                    selected = selected - 1
                end if
            case('B')  ! Down
                if (selected < current_count .and. current_count > 0) then
                    selected = selected + 1
                end if
            case('C')  ! Right - enter
                if (current_is_dir(selected)) then
                    if (trim(current_files(selected)) == "..") then
                        temp_dir = current_dir
                        current_dir = parent_dir
                        parent_dir = get_parent_path(current_dir)
                        selected = -1  ! Signal to find position in parent
                    else if (trim(current_files(selected)) /= ".") then
                        parent_dir = current_dir
                        current_dir = join_path(current_dir, current_files(selected))
                        selected = 1
                        scroll_offset = 0
                    end if
                end if
            case('D')  ! Left - back
                if (current_dir /= "/") then
                    temp_dir = current_dir
                    current_dir = parent_dir
                    parent_dir = get_parent_path(current_dir)
                    selected = -1  ! Signal to find position in parent
                end if
            end select
        case(113, 81)  ! 'q' or 'Q'
            running = .false.
        case(99, 67)  ! 'c' or 'C' - cd to directory on exit
            if (current_is_dir(selected)) then
                if (trim(current_files(selected)) == "..") then
                    exit_dir = parent_dir
                else if (trim(current_files(selected)) == ".") then
                    exit_dir = current_dir
                else
                    exit_dir = join_path(current_dir, current_files(selected))
                end if
                cd_on_exit = .true.
                running = .false.
            end if
        case(102, 70)  ! 'f' or 'F' - fzf search
            call fzf_search(current_dir, temp_dir)
            if (len_trim(temp_dir) > 0) then
                ! Navigate to the selected file's directory
                parent_dir = get_parent_path(temp_dir)
                current_dir = parent_dir
                parent_dir = get_parent_path(current_dir)
                selected = -2  ! Signal to find and center on fzf result
            end if
        case(65, 97)  ! 'A' or 'a' - git add
            if (in_git_repo .and. .not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    call git_add_file(current_dir, current_files(selected))
                end if
            end if
        case(77, 109)  ! 'M' or 'm' - git commit
            if (in_git_repo) then
                call git_commit_prompt(current_dir)
            end if
        case(85, 117)  ! 'U' or 'u' - git unstage (restore --staged)
            if (in_git_repo .and. .not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    ! Only unstage if file is actually staged
                    if (current_is_staged(selected)) then
                        call git_unstage_file(current_dir, current_files(selected))
                    end if
                end if
            end if
        end select
    end do

    ! Cleanup
    call execute_command_line("stty icanon echo 2>/dev/null")
    write(output_unit, '(a)', advance='no') CLEAR

    ! If cd_on_exit is set, write the directory to a temp file
    if (cd_on_exit) then
        call write_exit_dir(exit_dir)
    else
        write(output_unit, '(a)') "Thanks for using FORTRESS!"
    end if

contains

    function get_pwd() result(path)
        character(len=MAX_PATH) :: path
        integer :: unit, ios

        call execute_command_line("pwd > .fortress_pwd 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fortress_pwd", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)') path
            close(unit)
        else
            path = "."
        end if
        call execute_command_line("rm -f .fortress_pwd 2>/dev/null")
    end function get_pwd

    function get_parent_path(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=MAX_PATH) :: parent
        integer :: pos

        pos = index(path, "/", back=.true.)
        if (pos > 1) then
            parent = path(1:pos-1)
        else if (pos == 1) then
            parent = "/"
        else
            parent = "."
        end if
    end function get_parent_path

    function join_path(base, name) result(full)
        character(len=*), intent(in) :: base, name
        character(len=MAX_PATH) :: full

        if (base == "/") then
            full = "/" // trim(name)
        else
            full = trim(base) // "/" // trim(name)
        end if
    end function join_path

    function find_in_parent(dir, files, count) result(idx)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: idx, pos
        character(len=256) :: basename

        pos = index(dir, "/", back=.true.)
        if (pos > 0) then
            basename = dir(pos+1:)
        else
            basename = dir
        end if

        do idx = 1, count
            if (trim(files(idx)) == trim(basename)) return
        end do
        idx = 1
    end function find_in_parent

    subroutine get_file_list(dir, files, is_dir, is_exec, count)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(out) :: files
        logical, dimension(*), intent(out) :: is_dir, is_exec
        integer, intent(out) :: count
        integer :: unit, ios, stat
        character(len=MAX_PATH) :: fullpath

        call execute_command_line("ls -1a '" // trim(dir) // "' > .fortress_ls 2>/dev/null", wait=.true.)

        open(newunit=unit, file=".fortress_ls", status='old', iostat=ios)
        if (ios /= 0) then
            count = 0
            return
        end if

        count = 0
        do
            count = count + 1
            if (count > MAX_FILES) exit
            read(unit, '(a)', iostat=ios) files(count)
            if (ios /= 0) then
                count = count - 1
                exit
            end if

            fullpath = join_path(dir, files(count))
            call execute_command_line("test -d '" // trim(fullpath) // "'", exitstat=stat, wait=.true.)
            is_dir(count) = (stat == 0)

            ! Check if executable (but not directories)
            if (.not. is_dir(count)) then
                call execute_command_line("test -x '" // trim(fullpath) // "'", exitstat=stat, wait=.true.)
                is_exec(count) = (stat == 0)
            else
                is_exec(count) = .false.
            end if
        end do

        close(unit)
        call execute_command_line("rm -f .fortress_ls 2>/dev/null")
    end subroutine get_file_list

    subroutine get_term_size(r, c)
        integer, intent(out) :: r, c
        integer :: unit, ios

        call execute_command_line("tput lines > .fortress_size 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fortress_size", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *) r
            close(unit)
        else
            r = 24
        end if

        call execute_command_line("tput cols > .fortress_size 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fortress_size", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *) c
            close(unit)
        else
            c = 80
        end if

        call execute_command_line("rm -f .fortress_size 2>/dev/null")
    end subroutine get_term_size

    subroutine draw_interface(r, c)
        integer, intent(in) :: r, c
        integer :: left_w, i, parent_idx, current_idx, vis_h
        character(len=256) :: fname
        character(len=20) :: color_code

        left_w = c * 3 / 10
        vis_h = r - 3  ! Visible height

        ! Header
        write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir)

        ! Files (render based on scroll offsets)
        do i = 1, vis_h
            parent_idx = i + parent_scroll_offset
            current_idx = i + scroll_offset

            ! Parent pane
            if (parent_idx >= 1 .and. parent_idx <= parent_count) then
                fname = parent_files(parent_idx)
                if (parent_is_dir(parent_idx) .and. fname /= "." .and. fname /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for parent file
                color_code = get_file_color(parent_files(parent_idx), parent_is_dir(parent_idx), parent_is_exec(parent_idx))

                if (parent_idx == parent_selected) then
                    write(output_unit, '(a)', advance='no') DIM // BOLD // trim(color_code) // &
                        fname(1:min(len_trim(fname),left_w)) // RESET
                else
                    write(output_unit, '(a)', advance='no') DIM // trim(color_code) // &
                        fname(1:min(len_trim(fname),left_w)) // RESET
                end if
                write(output_unit, '(a)', advance='no') repeat(" ", max(0, left_w - len_trim(fname)))
            else
                write(output_unit, '(a)', advance='no') repeat(" ", left_w)
            end if

            ! Separator
            write(output_unit, '(a)', advance='no') " │ "

            ! Current pane
            if (current_idx >= 1 .and. current_idx <= current_count) then
                fname = current_files(current_idx)
                if (current_is_dir(current_idx) .and. fname /= "." .and. fname /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for current file
                color_code = get_file_color(current_files(current_idx), current_is_dir(current_idx), current_is_exec(current_idx))

                if (current_idx == selected) then
                    write(output_unit, '(a)', advance='no') REVERSE // trim(color_code) // trim(fname)
                    ! Add git indicators if in repo
                    if (in_git_repo) then
                        call write_git_indicators(current_is_staged(current_idx), &
                                                  current_is_unstaged(current_idx), &
                                                  current_is_untracked(current_idx), .true.)
                    end if
                    write(output_unit, '(a)') RESET
                else
                    write(output_unit, '(a)', advance='no') trim(color_code) // trim(fname)
                    ! Add git indicators if in repo
                    if (in_git_repo) then
                        call write_git_indicators(current_is_staged(current_idx), &
                                                  current_is_unstaged(current_idx), &
                                                  current_is_untracked(current_idx), .false.)
                    end if
                    write(output_unit, '(a)') RESET
                end if
            else
                write(output_unit, *)
            end if
        end do

        ! Footer
        if (in_git_repo) then
            write(output_unit, '(a)') DIM // trim(repo_name) // " | " // RESET // &
                                     DIM // "↑↓:nav →:enter ←:back f:find A:add U:unstage M:commit c:cd q:quit" // RESET
        else
            write(output_unit, '(a)') DIM // "↑↓:nav →:enter ←:back f:find c:cd q:quit" // RESET
        end if
    end subroutine draw_interface

    subroutine read_arrow_key(k)
        character(len=1), intent(out) :: k
        character(len=1) :: ch

        read(*, '(a1)', advance='no') ch
        if (ch == '[') then
            read(*, '(a1)', advance='no') k
        else
            k = ch
        end if
    end subroutine read_arrow_key

    function get_file_color(filename, is_dir, is_exec) result(color)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_dir, is_exec
        character(len=20) :: color

        ! Directories: Blue and bold
        if (is_dir) then
            color = BOLD // BLUE
        ! Dotfiles: Grey
        else if (filename(1:1) == '.') then
            color = GREY
        ! Executable files: Green
        else if (is_exec) then
            color = GREEN
        ! All other files: White
        else
            color = WHITE
        end if
    end function get_file_color

    subroutine write_exit_dir(dir)
        character(len=*), intent(in) :: dir
        character(len=MAX_PATH) :: temp_file
        integer :: unit, ios

        ! Create temp file in HOME directory
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_cd"

        open(newunit=unit, file=temp_file, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, '(a)') trim(dir)
            close(unit)
        end if
    end subroutine write_exit_dir

    subroutine fzf_search(search_dir, result_path)
        character(len=*), intent(in) :: search_dir
        character(len=*), intent(out) :: result_path
        character(len=MAX_PATH) :: temp_file, fzf_cmd
        integer :: unit, ios, stat

        result_path = ""

        ! Create temp file for fzf output
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_fzf"

        ! Restore terminal for fzf
        call execute_command_line("stty icanon echo 2>/dev/null")

        ! Build fzf command: find files, pipe to fzf, save selection
        fzf_cmd = "cd '" // trim(search_dir) // "' && " // &
                  "find . -type f -o -type d | " // &
                  "sed 's|^\./||' | " // &
                  "fzf --height=40% --reverse --border --preview 'ls -lh {}' " // &
                  "> " // trim(temp_file) // " 2>/dev/null"

        ! Run fzf
        call execute_command_line(trim(fzf_cmd), exitstat=stat, wait=.true.)

        ! Restore raw mode
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")

        ! Read result if fzf succeeded
        if (stat == 0) then
            open(newunit=unit, file=temp_file, status='old', iostat=ios)
            if (ios == 0) then
                read(unit, '(a)', iostat=ios) result_path
                if (ios == 0) then
                    ! Convert relative path to absolute
                    result_path = join_path(search_dir, result_path)
                end if
                close(unit)
            end if
        end if

        ! Cleanup
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
    end subroutine fzf_search

    function find_file_in_list(target_path, files, count) result(idx)
        character(len=*), intent(in) :: target_path
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: idx, pos
        character(len=MAX_PATH) :: basename

        ! Extract basename from target_path
        pos = index(target_path, "/", back=.true.)
        if (pos > 0) then
            basename = target_path(pos+1:)
        else
            basename = target_path
        end if

        ! Search for the file in the list
        do idx = 1, count
            if (trim(files(idx)) == trim(basename)) return
        end do

        ! Default to first item if not found
        idx = 1
    end function find_file_in_list

    subroutine detect_git_repo(dir, is_git, repo)
        character(len=*), intent(in) :: dir
        logical, intent(out) :: is_git
        character(len=*), intent(out) :: repo
        integer :: stat
        character(len=MAX_PATH) :: temp_file, git_dir

        is_git = .false.
        repo = ""

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
        end if
    end subroutine detect_git_repo

    subroutine get_git_status(dir, files, count, is_staged, is_unstaged, is_untracked)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        logical, dimension(*), intent(out) :: is_staged, is_unstaged, is_untracked
        character(len=MAX_PATH) :: temp_file, line, file_path, git_status
        integer :: unit, ios, stat, i, j
        character(len=MAX_PATH) :: full_path

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

        ! Note: git status will be refreshed in the next main loop iteration
    end subroutine git_add_file

    subroutine git_unstage_file(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=MAX_PATH*2) :: git_cmd
        integer :: stat

        ! Build git restore --staged command
        git_cmd = "cd '" // trim(dir) // "' && git restore --staged '" // trim(filename) // "' 2>/dev/null"
        call execute_command_line(trim(git_cmd), exitstat=stat, wait=.true.)

        ! Note: git status will be refreshed in the next main loop iteration
    end subroutine git_unstage_file

    subroutine git_commit_prompt(dir)
        character(len=*), intent(in) :: dir
        character(len=512) :: commit_msg
        character(len=MAX_PATH*2) :: git_cmd
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

end program fortress_clean
