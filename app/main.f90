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
            call get_git_status(current_dir, current_files, current_count, &
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

        ! Bounds check
        if (current_count > 0) then
            selected = max(1, min(selected, current_count))
        else
            selected = 1
        end if

        ! Find current dir in parent
        parent_selected = find_in_parent(current_dir, parent_files, parent_count)

        ! Adjust scroll to keep cursor visible
        if (selected < scroll_offset + 1) scroll_offset = max(0, selected - 1)
        if (selected > scroll_offset + visible_height) scroll_offset = selected - visible_height
        scroll_offset = max(0, min(scroll_offset, max(0, current_count - visible_height)))

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
                           in_git_repo, repo_name, branch_name)

        ! Get input
        read(*, '(a1)', advance='no') key

        ! Handle input
        select case(ichar(key))
        case(27)  ! ESC - arrow keys
            call read_arrow_key(key)
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
        case(113, 81)  ! 'q' or 'Q' - quit
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
        case(83, 115)  ! 'S' or 's' - fzf search (moved from 'f')
            call fzf_search(current_dir, temp_dir)
            if (len_trim(temp_dir) > 0) then
                parent_dir = get_parent_path(temp_dir)
                current_dir = parent_dir
                parent_dir = get_parent_path(current_dir)
                selected = -2
                call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
            end if
        case(65, 97)  ! 'A' or 'a' - git add
            if (in_git_repo .and. .not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    call git_add_file(current_dir, current_files(selected))
                end if
            end if
        case(85, 117)  ! 'U' or 'u' - git unstage
            if (in_git_repo .and. .not. current_is_dir(selected)) then
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
        case(68, 100)  ! 'D' or 'd' - show git diff
            if (in_git_repo .and. .not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    if (current_is_staged(selected) .or. current_is_unstaged(selected)) then
                        call show_git_diff_fullscreen(current_dir, current_files(selected), &
                                                      current_is_staged(selected), current_is_unstaged(selected))
                    end if
                end if
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

end program fortress
