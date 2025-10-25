program fortress
    use iso_fortran_env, only: output_unit
    use terminal_control
    use filesystem_ops
    use git_ops
    use ui_display
    implicit none

    ! State variables
    character(len=MAX_PATH) :: current_dir, parent_dir, temp_dir, exit_dir
    character(len=MAX_PATH), dimension(MAX_FILES) :: current_files, parent_files
    logical, dimension(MAX_FILES) :: current_is_dir, parent_is_dir
    logical, dimension(MAX_FILES) :: current_is_exec, parent_is_exec
    logical, dimension(MAX_FILES) :: current_is_staged, current_is_unstaged, current_is_untracked
    logical, dimension(MAX_FILES) :: parent_is_staged, parent_is_unstaged, parent_is_untracked
    logical, dimension(MAX_FILES) :: current_has_incoming
    integer :: current_count, parent_count
    integer :: selected = 1, parent_selected = -1
    integer :: scroll_offset = 0, parent_scroll_offset = 0
    character(len=256) :: repo_name, branch_name
    logical :: in_git_repo = .false., running = .true., cd_on_exit = .false.
    logical :: show_dotfiles = .true.

    ! Move mode state
    logical :: move_mode = .false.
    character(len=MAX_PATH) :: move_source_path
    character(len=MAX_PATH) :: move_source_name
    integer :: move_dest_selected = 1

    character(len=1) :: key
    integer :: i, rows, cols, visible_height

    ! Initialize
    current_dir = get_pwd()
    parent_dir = get_parent_path(current_dir)
    call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
    call setup_raw_mode()

    ! Main loop
    do while (running)
        ! Get files
        call get_file_list(current_dir, current_files, current_is_dir, current_is_exec, current_count)
        call get_file_list(parent_dir, parent_files, parent_is_dir, parent_is_exec, parent_count)

        ! Filter dotfiles if needed
        if (.not. show_dotfiles) then
            call filter_dotfiles(current_files, current_is_dir, current_is_exec, current_count)
            call filter_dotfiles(parent_files, parent_is_dir, parent_is_exec, parent_count)
        end if

        ! Initialize git arrays - only for actual file counts
        do i = 1, current_count
            current_is_staged(i) = .false.
            current_is_unstaged(i) = .false.
            current_is_untracked(i) = .false.
            current_has_incoming(i) = .false.
        end do
        do i = 1, parent_count
            parent_is_staged(i) = .false.
            parent_is_unstaged(i) = .false.
            parent_is_untracked(i) = .false.
        end do

        ! Get git status if in a repo
        if (in_git_repo) then
            call get_git_status(current_dir, current_files, current_is_dir, current_count, &
                               current_is_staged, current_is_unstaged, current_is_untracked)
            call mark_incoming_changes(current_dir, current_files, current_count, current_has_incoming)
        end if

        ! Get terminal size
        call get_term_size(rows, cols)
        visible_height = rows - 3

        ! Handle navigation signals from previous iteration
        if (selected == -1) then
            selected = find_in_parent(temp_dir, current_files, current_count)
            scroll_offset = max(0, selected - visible_height / 2)
        else if (selected == -2) then
            selected = find_file_in_list(temp_dir, current_files, current_count)
            scroll_offset = max(0, selected - visible_height / 2)
        end if

        ! Handle move mode destination cursor
        if (move_mode .and. move_dest_selected == -1) then
            move_dest_selected = find_in_parent(temp_dir, current_files, current_count)
        end if

        ! Bounds check
        if (current_count > 0) then
            selected = max(1, min(selected, current_count))
            if (move_mode) then
                move_dest_selected = max(1, min(move_dest_selected, current_count))
            end if
        else
            selected = 1
            if (move_mode) move_dest_selected = 1
        end if

        ! Find current dir in parent
        parent_selected = find_in_parent(current_dir, parent_files, parent_count)

        ! Adjust scroll to keep cursor visible
        if (move_mode) then
            ! In move mode, track the destination cursor
            if (move_dest_selected < scroll_offset + 1) scroll_offset = max(0, move_dest_selected - 1)
            if (move_dest_selected > scroll_offset + visible_height) scroll_offset = move_dest_selected - visible_height
            scroll_offset = max(0, min(scroll_offset, max(0, current_count - visible_height)))
        else
            ! Normal mode, track the selection cursor
            if (selected < scroll_offset + 1) scroll_offset = max(0, selected - 1)
            if (selected > scroll_offset + visible_height) scroll_offset = selected - visible_height
            scroll_offset = max(0, min(scroll_offset, max(0, current_count - visible_height)))
        end if

        if (parent_selected > 0) then
            if (parent_selected < parent_scroll_offset + 1) parent_scroll_offset = max(0, parent_selected - 1)
            if (parent_selected > parent_scroll_offset + visible_height) parent_scroll_offset = parent_selected - visible_height
            parent_scroll_offset = max(0, min(parent_scroll_offset, max(0, parent_count - visible_height)))
        end if

        ! Draw
        write(output_unit, '(a)', advance='no') CLEAR
        call draw_interface(rows, cols, current_dir, current_files, current_is_dir, current_is_exec, &
                           current_is_staged, current_is_unstaged, current_is_untracked, current_has_incoming, &
                           current_count, parent_files, parent_is_dir, parent_is_exec, parent_count, &
                           selected, parent_selected, scroll_offset, parent_scroll_offset, &
                           in_git_repo, repo_name, branch_name, &
                           move_mode, move_source_name, move_dest_selected)

        ! Get input (with error handling for End-of-record after Enter key)
        read(*, '(a1)', advance='no', iostat=i) key
        ! Only cycle on End-of-record (negative iostat), which happens after pressing Enter
        ! Don't skip on positive errors or when we successfully read a character
        if (i < 0) cycle  ! End-of-record - skip and try again
        if (i > 0) cycle  ! Other read errors - skip and try again

        ! Handle input
        select case(ichar(key))
        case(27)  ! ESC - arrow keys
            call read_arrow_key(key)

            if (move_mode) then
                ! In move mode, navigate directories only
                select case(key)
                case('A')  ! Up - jump to previous directory
                    move_dest_selected = find_prev_directory(current_files, current_is_dir, current_count, move_dest_selected)
                case('B')  ! Down - jump to next directory
                    move_dest_selected = find_next_directory(current_files, current_is_dir, current_count, move_dest_selected)
                case('C')  ! Right - enter directory
                    if (current_is_dir(move_dest_selected)) then
                        if (trim(current_files(move_dest_selected)) == "..") then
                            ! Don't descend into ..
                        else if (trim(current_files(move_dest_selected)) /= ".") then
                            ! Descend into directory
                            parent_dir = current_dir
                            current_dir = join_path(current_dir, current_files(move_dest_selected))
                            move_dest_selected = find_first_directory(current_files, current_is_dir, current_count)
                            scroll_offset = 0
                            call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                        end if
                    end if
                case('D')  ! Left - go to parent
                    if (current_dir /= "/") then
                        temp_dir = current_dir
                        current_dir = parent_dir
                        parent_dir = get_parent_path(current_dir)
                        move_dest_selected = -1  ! Will be set to parent dir position
                        call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                    end if
                end select
            else
                ! Normal navigation
                select case(key)
                case('A')  ! Up
                    if (selected > 1) selected = selected - 1
                case('B')  ! Down
                    if (selected < current_count .and. current_count > 0) selected = selected + 1
                case('C')  ! Right - enter directory
                    if (current_is_dir(selected)) then
                        if (trim(current_files(selected)) == "..") then
                            temp_dir = current_dir
                            current_dir = parent_dir
                            parent_dir = get_parent_path(current_dir)
                            selected = -1
                            call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                        else if (trim(current_files(selected)) /= ".") then
                            parent_dir = current_dir
                            current_dir = join_path(current_dir, current_files(selected))
                            selected = 1
                            scroll_offset = 0
                            call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                        end if
                    end if
                case('D')  ! Left - go back
                    if (current_dir /= "/") then
                        temp_dir = current_dir
                        current_dir = parent_dir
                        parent_dir = get_parent_path(current_dir)
                        selected = -1
                        call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                    end if
                end select
            end if
        case(113, 81)  ! 'q' or 'Q' - exit move mode or quit
            if (move_mode) then
                move_mode = .false.
            else
                running = .false.
            end if
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
        case(83, 115)  ! 'S' or 's' - fzf search (moved from 'f')
            call fzf_search(current_dir, temp_dir)
            if (len_trim(temp_dir) > 0) then
                parent_dir = get_parent_path(temp_dir)
                current_dir = parent_dir
                parent_dir = get_parent_path(current_dir)
                selected = -2
                call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
            end if
        case(65, 97)  ! 'A' or 'a' - git add (batch stage directories)
            if (in_git_repo) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    call git_add_file(current_dir, current_files(selected))
                end if
            end if
        case(85, 117)  ! 'U' or 'u' - git unstage (batch unstage directories)
            if (in_git_repo) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    if (current_is_staged(selected)) then
                        call git_unstage_file(current_dir, current_files(selected))
                    end if
                end if
            end if
        case(77, 109)  ! 'M' or 'm' - git commit
            if (in_git_repo) then
                call git_commit_prompt(current_dir, repo_name)
            end if
        case(80, 112)  ! 'P' or 'p' - git push
            if (in_git_repo) then
                call git_push_prompt(current_dir, repo_name)
            end if
        case(84, 116)  ! 'T' or 't' - git tag
            if (in_git_repo) then
                call git_tag_prompt(current_dir, repo_name)
            end if
        case(70, 102)  ! 'F' or 'f' - git fetch
            if (in_git_repo) then
                call git_fetch_prompt(current_dir, repo_name)
            end if
        case(76, 108)  ! 'L' or 'l' - git pull
            if (in_git_repo) then
                call git_pull_prompt(current_dir, repo_name)
            end if
        case(79, 111)  ! 'O' or 'o' - open file
            if (.not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    call open_file_in_default_app(join_path(current_dir, current_files(selected)))
                end if
            end if
        case(78, 110)  ! 'N' or 'n' - rename file/directory
            if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                call rename_file_prompt(current_dir, current_files(selected))
            end if
        case(68, 100)  ! 'D' or 'd' - show git diff
            if (in_git_repo .and. .not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    if (current_is_staged(selected) .or. current_is_unstaged(selected)) then
                        call show_git_diff_fullscreen(current_dir, current_files(selected), &
                                                      current_is_staged(selected), current_is_unstaged(selected))
                    end if
                end if
            end if
        case(46)  ! '.' - toggle dotfiles visibility
            show_dotfiles = .not. show_dotfiles
            ! Reset selection to avoid going out of bounds
            selected = 1
            scroll_offset = 0
        case(86, 118)  ! 'V' or 'v' - enter move mode OR confirm move
            if (move_mode) then
                ! Confirm move - execute the move to the white-highlighted directory
                call execute_move_file(move_source_path, current_dir, current_files(move_dest_selected), &
                                      current_is_dir(move_dest_selected))
                move_mode = .false.
            else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                ! Enter move mode - store source file or directory
                move_source_path = join_path(current_dir, current_files(selected))
                move_source_name = current_files(selected)
                move_mode = .true.
                ! Find first directory for destination cursor
                move_dest_selected = find_first_directory(current_files, current_is_dir, current_count)
            end if
        end select
    end do

    ! Cleanup
    call restore_terminal()
    write(output_unit, '(a)', advance='no') CLEAR

    if (cd_on_exit) then
        call write_exit_dir(exit_dir)
    else
        write(output_unit, '(a)') "Thanks for using FORTRESS!"
    end if

contains

    subroutine filter_dotfiles(files, is_dir, is_exec, count)
        character(len=*), dimension(*), intent(inout) :: files
        logical, dimension(*), intent(inout) :: is_dir, is_exec
        integer, intent(inout) :: count
        character(len=MAX_PATH), dimension(MAX_FILES) :: temp_files
        logical, dimension(MAX_FILES) :: temp_is_dir, temp_is_exec
        integer :: i, new_count

        new_count = 0
        do i = 1, count
            ! Always keep "." and "..", filter other dotfiles
            if (trim(files(i)) == "." .or. trim(files(i)) == ".." .or. files(i)(1:1) /= '.') then
                new_count = new_count + 1
                temp_files(new_count) = files(i)
                temp_is_dir(new_count) = is_dir(i)
                temp_is_exec(new_count) = is_exec(i)
            end if
        end do

        ! Copy back
        do i = 1, new_count
            files(i) = temp_files(i)
            is_dir(i) = temp_is_dir(i)
            is_exec(i) = temp_is_exec(i)
        end do
        count = new_count
    end subroutine filter_dotfiles

    function find_first_directory(files, is_dir, count) result(idx)
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir
        integer, intent(in) :: count
        integer :: idx, i

        ! Find first directory (including . and ..)
        do i = 1, count
            if (is_dir(i)) then
                idx = i
                return
            end if
        end do

        ! If no directory found, default to first item
        idx = 1
    end function find_first_directory

    function find_next_directory(files, is_dir, count, current) result(idx)
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir
        integer, intent(in) :: count, current
        integer :: idx, i

        ! Search forward from current position (including . and ..)
        do i = current + 1, count
            if (is_dir(i)) then
                idx = i
                return
            end if
        end do

        ! No directory found forward, stay at current
        idx = current
    end function find_next_directory

    function find_prev_directory(files, is_dir, count, current) result(idx)
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir
        integer, intent(in) :: count, current
        integer :: idx, i

        ! Search backward from current position (including . and ..)
        do i = current - 1, 1, -1
            if (is_dir(i)) then
                idx = i
                return
            end if
        end do

        ! No directory found backward, stay at current
        idx = current
    end function find_prev_directory

    subroutine execute_move_file(source_path, dest_dir, dest_name, is_dest_dir)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD
        character(len=*), intent(in) :: source_path, dest_dir, dest_name
        logical, intent(in) :: is_dest_dir
        character(len=MAX_PATH*2) :: dest_path, mv_cmd
        integer :: stat

        ! Build destination path
        if (is_dest_dir) then
            if (trim(dest_name) == ".") then
                ! Move to current directory
                dest_path = dest_dir
            else if (trim(dest_name) == "..") then
                ! Move to parent directory
                dest_path = get_parent_path(dest_dir)
            else
                ! Move into the selected directory
                dest_path = join_path(dest_dir, dest_name)
            end if
        else
            ! Not a directory - shouldn't happen due to our navigation, but handle it
            dest_path = dest_dir
        end if

        ! Execute move command (mv will move file into dest_path directory)
        mv_cmd = "mv '" // trim(source_path) // "' '" // trim(dest_path) // "'"
        call execute_command_line(trim(mv_cmd), exitstat=stat, wait=.true.)

        ! Show result briefly (no user input to avoid terminal state issues)
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)') BOLD // "Move Result" // RESET
        write(output_unit, *)
        if (stat == 0) then
            write(output_unit, '(a)') GREEN // "✓ Moved successfully!" // RESET
            write(output_unit, '(a)') "  From: " // trim(source_path)
            write(output_unit, '(a)') "  To:   " // trim(dest_path)
        else
            write(output_unit, '(a)') RED // "✗ Move failed" // RESET
            write(output_unit, '(a)') "  (destination may already exist or be invalid)"
        end if
        write(output_unit, *)

        ! Brief pause to let user see the result (use Fortran sleep to avoid stdin issues)
        call sleep(2)
    end subroutine execute_move_file

end program fortress
