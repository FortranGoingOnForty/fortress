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

    ! Mode state (normal or git)
    character(len=10) :: mode = 'normal'

    ! Fuzzy search state
    character(len=32) :: search_buffer = ''
    integer :: search_length = 0
    integer(8) :: last_search_tick = 0
    integer(8) :: clock_rate, current_tick

    ! Move mode state
    logical :: move_mode = .false.
    character(len=MAX_PATH) :: move_source_path
    character(len=MAX_PATH) :: move_source_name
    integer :: move_dest_selected = 1

    ! Clipboard state
    logical :: has_clipboard = .false.
    logical :: clipboard_is_cut = .false.  ! true = cut, false = copy
    character(len=MAX_PATH) :: clipboard_source_path
    character(len=MAX_PATH) :: clipboard_source_name

    ! Multi-select state
    logical, dimension(MAX_FILES) :: is_selected
    integer :: selection_count = 0
    logical :: in_selection_mode = .false.
    integer :: selection_anchor = -1  ! For shift+arrow block selection
    logical :: has_disjoint_selection = .false.  ! True if non-contiguous items selected

    ! Multi-clipboard for batch operations
    character(len=MAX_PATH), dimension(MAX_FILES) :: clipboard_paths
    character(len=MAX_PATH), dimension(MAX_FILES) :: clipboard_names
    integer :: clipboard_count = 0

    ! Favorites/bookmarks state
    character(len=MAX_PATH), dimension(10) :: favorite_dirs
    integer :: favorite_count = 0
    logical, dimension(MAX_FILES) :: current_is_favorite, parent_is_favorite

    ! Rename mode state
    logical :: in_rename_mode = .false.
    character(len=MAX_PATH) :: rename_buffer = ''
    integer :: rename_cursor_pos = 0

    character(len=1) :: key
    integer :: i, rows, cols, visible_height, top_padding
    logical :: is_shift_pressed, is_alt_pressed
    character(len=256) :: term_program

    ! Initialize
    current_dir = get_pwd()
    parent_dir = get_parent_path(current_dir)
    call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
    call load_favorites(favorite_dirs, favorite_count)
    call system_clock(count_rate=clock_rate)

    ! Detect terminal type once for consistent padding throughout
    call get_environment_variable("TERM_PROGRAM", term_program)
    if (index(term_program, "WezTerm") > 0 .or. index(term_program, "ghostty") > 0) then
        top_padding = 1  ! WezTerm/Ghostty: 1 line padding (minimal but keeps FORTRESS visible)
    else if (index(term_program, "Apple_Terminal") > 0 .or. index(term_program, "iTerm") > 0) then
        top_padding = 2  ! Terminal.app and iTerm2 need 2 lines
    else
        top_padding = 1  ! Other terminals need 1 line
    end if

    call setup_raw_mode()
    call enter_alt_screen()  ! Use alternate screen buffer to prevent scrolling issues
    call hide_cursor()  ! Hide cursor for cleaner display

    ! Clear screen and position at home
    write(output_unit, '(a)', advance='no') ESC // "[H"   ! Move to home (1,1)
    write(output_unit, '(a)', advance='no') ESC // "[J"   ! Clear from cursor to end
    flush(output_unit)

    ! Initialize selection array to false
    do i = 1, MAX_FILES
        is_selected(i) = .false.
    end do

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

        ! Initialize git arrays and selection - only for actual file counts
        do i = 1, current_count
            current_is_staged(i) = .false.
            current_is_unstaged(i) = .false.
            current_is_untracked(i) = .false.
            current_has_incoming(i) = .false.
            ! Keep selections if still in same directory, clear otherwise
            if (i > MAX_FILES) then
                is_selected(i) = .false.
            end if
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

        ! Mark favorited directories
        call mark_favorites_in_lists(current_dir, current_files, current_count, current_is_dir, &
                                     favorite_dirs, favorite_count, current_is_favorite)
        call mark_favorites_in_lists(parent_dir, parent_files, parent_count, parent_is_dir, &
                                     favorite_dirs, favorite_count, parent_is_favorite)

        ! Get terminal size and calculate visible height accounting for padding and 2-line header
        ! Layout: top_padding + header(2 lines) + vis_h + footer(1 line) + buffer = rows
        ! Subtract 1 extra to prevent any scrolling: vis_h = rows - top_padding - 4
        call get_term_size(rows, cols)
        visible_height = rows - top_padding - 4

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

        ! Draw - position at 1,1 then clear from cursor to end (prevents scrollback)
        write(output_unit, '(a)', advance='no') ESC // "[H"   ! Move to home (1,1)
        write(output_unit, '(a)', advance='no') ESC // "[J"   ! Clear from cursor to end of screen
        flush(output_unit)
        call draw_interface(rows, cols, top_padding, current_dir, current_files, current_is_dir, current_is_exec, &
                           current_is_staged, current_is_unstaged, current_is_untracked, current_has_incoming, &
                           current_count, parent_files, parent_is_dir, parent_is_exec, parent_count, &
                           selected, parent_selected, scroll_offset, parent_scroll_offset, &
                           in_git_repo, repo_name, branch_name, mode, &
                           move_mode, move_source_name, move_dest_selected, &
                           has_clipboard, clipboard_is_cut, clipboard_source_name, clipboard_count, &
                           is_selected, selection_count, &
                           current_is_favorite, parent_is_favorite, &
                           search_buffer, search_length, &
                           in_rename_mode, rename_buffer, rename_cursor_pos)

        ! Get input (with error handling for End-of-record after Enter key)
        read(*, '(a1)', advance='no', iostat=i) key

        ! Rename mode key handling - intercept ALL keys when in rename mode (BEFORE error handling)
        if (in_rename_mode) then
            ! In rename mode, handle read errors differently
            if (i < 0) then
                ! End-of-record in rename mode - treat as Enter
                key = achar(13)
            else if (i > 0) then
                ! Read error - ignore and continue
                cycle
            end if

            ! Handle ESC to cancel rename
            if (ichar(key) == 27) then
                in_rename_mode = .false.
                rename_buffer = ''
                rename_cursor_pos = 0
                cycle
            ! Handle Enter to confirm rename
            else if (ichar(key) == 10 .or. ichar(key) == 13) then
                ! Execute rename if name changed
                if (len_trim(rename_buffer) > 0 .and. trim(rename_buffer) /= trim(current_files(selected))) then
                    block
                        character(len=MAX_PATH) :: old_path, new_path, old_name
                        character(len=MAX_PATH*2) :: mv_cmd
                        integer :: stat

                        ! Store the old name for cursor tracking
                        old_name = current_files(selected)
                        old_path = join_path(current_dir, old_name)
                        new_path = join_path(current_dir, trim(rename_buffer))

                        ! Use -f flag for case-only renames, double quotes for paths
                        mv_cmd = 'mv -f "' // trim(old_path) // '" "' // trim(new_path) // '"'
                        call execute_command_line(trim(mv_cmd), exitstat=stat, wait=.true.)

                        ! After rename succeeds, find the renamed file in the refreshed list
                        if (stat == 0) then
                            temp_dir = new_path
                            selected = -2  ! Signal to find this file in the next iteration
                        end if
                    end block
                end if
                in_rename_mode = .false.
                rename_buffer = ''
                rename_cursor_pos = 0
                cycle
            ! Handle Backspace
            else if (ichar(key) == 127 .or. ichar(key) == 8) then
                if (rename_cursor_pos > 0) then
                    if (rename_cursor_pos == len_trim(rename_buffer)) then
                        ! Cursor at end - simple delete
                        rename_buffer = rename_buffer(1:len_trim(rename_buffer)-1)
                    else
                        ! Cursor in middle - delete and shift left
                        rename_buffer = rename_buffer(1:rename_cursor_pos-1) // &
                                       rename_buffer(rename_cursor_pos+1:len_trim(rename_buffer))
                    end if
                    rename_cursor_pos = rename_cursor_pos - 1
                end if
                cycle
            ! Handle printable characters - insert at cursor position
            else if ((ichar(key) >= ichar('a') .and. ichar(key) <= ichar('z')) .or. &
                     (ichar(key) >= ichar('A') .and. ichar(key) <= ichar('Z')) .or. &
                     (ichar(key) >= ichar('0') .and. ichar(key) <= ichar('9')) .or. &
                     key == '_' .or. key == '-' .or. key == '.' .or. key == ' ') then
                if (len_trim(rename_buffer) < MAX_PATH - 1) then
                    if (rename_cursor_pos == len_trim(rename_buffer)) then
                        ! Cursor at end - simple append
                        rename_buffer = trim(rename_buffer) // key
                    else
                        ! Cursor in middle - insert and shift right
                        rename_buffer = rename_buffer(1:rename_cursor_pos) // key // &
                                       rename_buffer(rename_cursor_pos+1:len_trim(rename_buffer))
                    end if
                    rename_cursor_pos = rename_cursor_pos + 1
                end if
                cycle
            end if
            ! Ignore all other keys in rename mode
            cycle
        end if

        ! Handle read errors for normal mode
        if (i < 0) cycle  ! End-of-record - skip and try again
        if (i > 0) cycle  ! Other read errors - skip and try again

        ! Fuzzy search: handle printable characters (only in normal mode, no special modes active)
        if (trim(mode) == 'normal' .and. .not. move_mode .and. selection_count == 0) then
            ! Check if character is printable (letters, digits, dash, underscore, dot)
            if ((ichar(key) >= ichar('a') .and. ichar(key) <= ichar('z')) .or. &
                (ichar(key) >= ichar('A') .and. ichar(key) <= ichar('Z')) .or. &
                (ichar(key) >= ichar('0') .and. ichar(key) <= ichar('9')) .or. &
                key == '-' .or. key == '_' .or. key == '.') then

                ! Check timeout (0.5 seconds = clock_rate / 2)
                if (search_length > 0) then
                    call system_clock(current_tick)
                    if (current_tick - last_search_tick > clock_rate / 2) then
                        ! Timeout - clear buffer and start fresh
                        search_length = 0
                        search_buffer = ''
                    end if
                end if

                ! Add to buffer
                if (search_length < 32) then
                    search_length = search_length + 1
                    search_buffer(search_length:search_length) = key
                    call system_clock(last_search_tick)

                    ! Perform fuzzy jump
                    call fuzzy_jump(current_files, current_count, search_buffer(1:search_length), selected)
                end if
                cycle  ! Skip normal key processing
            else if (ichar(key) == 127 .or. ichar(key) == 8) then
                ! Backspace - remove last character from search
                if (search_length > 0) then
                    search_length = search_length - 1
                    call system_clock(last_search_tick)
                    if (search_length > 0) then
                        call fuzzy_jump(current_files, current_count, search_buffer(1:search_length), selected)
                    end if
                end if
                cycle
            end if
        end if

        ! Handle input
        select case(ichar(key))
        case(27)  ! ESC - arrow keys, Shift+arrow keys, or Alt+key
            ! Clear search buffer if active (but still process the arrow key)
            if (search_length > 0) then
                search_length = 0
                search_buffer = ''
            end if

            call read_key_with_modifiers(key, is_shift_pressed, is_alt_pressed)

            ! Handle Alt+key combinations
            if (is_alt_pressed) then
                ! Process Alt keys here (key is now encoded as achar(1-26))
                select case(ichar(key))
                case(7)  ! Alt+g - toggle git mode
                    if (in_git_repo) then
                        if (mode == 'normal') then
                            mode = 'git'
                        else
                            mode = 'normal'
                        end if
                    end if
                case(14)  ! Alt+n - enter rename mode
                    if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                        in_rename_mode = .true.
                        rename_buffer = current_files(selected)
                        rename_cursor_pos = len_trim(rename_buffer)
                    end if
                case(22)  ! Alt+v - view file
                    if (.not. current_is_dir(selected)) then
                        if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                            call open_file_in_default_app(join_path(current_dir, current_files(selected)))
                        end if
                    end if
                case(19)  ! Alt+s - fzf search
                    call fzf_search(current_dir, temp_dir)
                    if (len_trim(temp_dir) > 0) then
                        parent_dir = get_parent_path(temp_dir)
                        current_dir = parent_dir
                        parent_dir = get_parent_path(current_dir)
                        selected = -2
                        call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                    end if
                case(3)  ! Alt+c - cd on exit
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
                case(18)  ! Alt+r - delete
                    if (selection_count > 0) then
                        call delete_multi_with_confirmation(current_dir, current_files, current_is_dir, &
                                                           is_selected, selection_count, current_count)
                        call clear_all_selections(is_selected, selection_count, in_selection_mode)
                    else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                        call delete_with_confirmation(current_dir, current_files(selected), current_is_dir(selected))
                    end if
                case(13)  ! Alt+m - move mode
                    if (move_mode) then
                        call execute_move_file(move_source_path, current_dir, current_files(move_dest_selected), &
                                              current_is_dir(move_dest_selected))
                        move_mode = .false.
                    else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                        move_source_path = join_path(current_dir, current_files(selected))
                        move_source_name = current_files(selected)
                        move_mode = .true.
                        move_dest_selected = find_first_directory(current_files, current_is_dir, current_count)
                    end if
                case(25)  ! Alt+y - copy
                    if (selection_count > 0) then
                        clipboard_count = 0
                        do i = 1, current_count
                            if (is_selected(i) .and. trim(current_files(i)) /= "." .and. &
                                trim(current_files(i)) /= "..") then
                                clipboard_count = clipboard_count + 1
                                clipboard_paths(clipboard_count) = join_path(current_dir, current_files(i))
                                clipboard_names(clipboard_count) = current_files(i)
                            end if
                        end do
                        clipboard_is_cut = .false.
                        has_clipboard = .true.
                        call clear_all_selections(is_selected, selection_count, in_selection_mode)
                    else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                        clipboard_count = 1
                        clipboard_paths(1) = join_path(current_dir, current_files(selected))
                        clipboard_names(1) = current_files(selected)
                        clipboard_source_path = clipboard_paths(1)
                        clipboard_source_name = clipboard_names(1)
                        clipboard_is_cut = .false.
                        has_clipboard = .true.
                    end if
                case(24)  ! Alt+x - cut
                    if (selection_count > 0) then
                        clipboard_count = 0
                        do i = 1, current_count
                            if (is_selected(i) .and. trim(current_files(i)) /= "." .and. &
                                trim(current_files(i)) /= "..") then
                                clipboard_count = clipboard_count + 1
                                clipboard_paths(clipboard_count) = join_path(current_dir, current_files(i))
                                clipboard_names(clipboard_count) = current_files(i)
                            end if
                        end do
                        clipboard_is_cut = .true.
                        has_clipboard = .true.
                        call clear_all_selections(is_selected, selection_count, in_selection_mode)
                    else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                        clipboard_count = 1
                        clipboard_paths(1) = join_path(current_dir, current_files(selected))
                        clipboard_names(1) = current_files(selected)
                        clipboard_source_path = clipboard_paths(1)
                        clipboard_source_name = clipboard_names(1)
                        clipboard_is_cut = .true.
                        has_clipboard = .true.
                    end if
                case(16)  ! Alt+p - paste
                    if (has_clipboard) then
                        if (clipboard_count > 1) then
                            call execute_multi_paste(clipboard_paths, clipboard_names, clipboard_count, &
                                                    clipboard_is_cut, current_dir, current_files(selected), &
                                                    current_is_dir(selected))
                        else
                            call execute_paste(clipboard_paths(1), clipboard_is_cut, current_dir, &
                                              current_files(selected), current_is_dir(selected))
                        end if
                        if (clipboard_is_cut) then
                            has_clipboard = .false.
                            clipboard_count = 0
                        end if
                    end if
                end select
                ! Alt key processed, skip rest of case(27)
                cycle
            end if

            ! Handle arrow keys (only if not Alt key)
            if (.true.) then
                ! Process arrow keys normally
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
                    if (is_shift_pressed .and. .not. has_disjoint_selection) then
                        ! Shift+Up: Start or extend block selection upward
                        if (selection_anchor == -1) then
                            ! Start new block selection
                            selection_anchor = selected
                            call clear_all_selections(is_selected, selection_count, in_selection_mode)
                        end if
                        if (selected > 1) selected = selected - 1
                        ! Select range from anchor to current
                        call select_range(is_selected, selection_count, selection_anchor, selected, &
                                         current_files, current_count)
                    else
                        ! Normal up movement - clear anchor
                        if (selected > 1) selected = selected - 1
                        selection_anchor = -1
                    end if
                case('B')  ! Down
                    if (is_shift_pressed .and. .not. has_disjoint_selection) then
                        ! Shift+Down: Start or extend block selection downward
                        if (selection_anchor == -1) then
                            ! Start new block selection
                            selection_anchor = selected
                            call clear_all_selections(is_selected, selection_count, in_selection_mode)
                        end if
                        if (selected < current_count .and. current_count > 0) selected = selected + 1
                        ! Select range from anchor to current
                        call select_range(is_selected, selection_count, selection_anchor, selected, &
                                         current_files, current_count)
                    else
                        ! Normal down movement - clear anchor
                        if (selected < current_count .and. current_count > 0) selected = selected + 1
                        selection_anchor = -1
                    end if
                case('C')  ! Right - enter directory
                    if (current_is_dir(selected)) then
                        if (trim(current_files(selected)) == "..") then
                            temp_dir = current_dir
                            current_dir = parent_dir
                            parent_dir = get_parent_path(current_dir)
                            selected = -1
                            selection_anchor = -1
                            call clear_all_selections(is_selected, selection_count, in_selection_mode)
                            call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                        else if (trim(current_files(selected)) /= ".") then
                            parent_dir = current_dir
                            current_dir = join_path(current_dir, current_files(selected))
                            selected = 1
                            scroll_offset = 0
                            selection_anchor = -1
                            call clear_all_selections(is_selected, selection_count, in_selection_mode)
                            call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                        end if
                    end if
                case('D')  ! Left - go back
                    if (current_dir /= "/") then
                        temp_dir = current_dir
                        current_dir = parent_dir
                        parent_dir = get_parent_path(current_dir)
                        selected = -1
                        selection_anchor = -1
                        call clear_all_selections(is_selected, selection_count, in_selection_mode)
                        call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
                    end if
                end select
            end if
            end if  ! End of .not. is_alt_pressed check
        case(7)  ! Alt+g - toggle git mode
            if (in_git_repo) then
                if (mode == 'normal') then
                    mode = 'git'
                else
                    mode = 'normal'
                end if
            end if
        case(14)  ! Alt+n - enter rename mode
            if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                ! Enter rename mode - pre-fill with current filename
                in_rename_mode = .true.
                rename_buffer = current_files(selected)
                rename_cursor_pos = len_trim(rename_buffer)
            end if
        case(22)  ! Alt+v - view file (always available)
            if (.not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    call open_file_in_default_app(join_path(current_dir, current_files(selected)))
                end if
            end if
        case(113, 81)  ! 'q' or 'Q' - exit move mode
            if (move_mode) then
                move_mode = .false.
            end if
        case(17)  ! Ctrl-q - quit program
            running = .false.
        case(3)  ! Alt+c - cd to directory on exit
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
        case(19)  ! Alt+s - fzf search
            call fzf_search(current_dir, temp_dir)
            if (len_trim(temp_dir) > 0) then
                parent_dir = get_parent_path(temp_dir)
                current_dir = parent_dir
                parent_dir = get_parent_path(current_dir)
                selected = -2
                call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
            end if
        case(65, 97)  ! 'A' or 'a' - git add (batch stage directories)
            if (in_git_repo .and. trim(mode) == 'git') then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    call git_add_file(current_dir, current_files(selected))
                end if
            end if
        case(85, 117)  ! 'U' or 'u' - git unstage (batch unstage directories)
            if (in_git_repo .and. trim(mode) == 'git') then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    if (current_is_staged(selected)) then
                        call git_unstage_file(current_dir, current_files(selected))
                    end if
                end if
            end if
        case(77, 109)  ! 'M' or 'm' - git commit
            if (in_git_repo .and. trim(mode) == 'git') then
                call git_commit_prompt(current_dir, repo_name)
            end if
        case(72, 104)  ! 'H' or 'h' - git push (h for "push to remote Host")
            if (in_git_repo .and. trim(mode) == 'git') then
                call git_push_prompt(current_dir, repo_name)
            end if
        case(84, 116)  ! 'T' or 't' - git tag
            if (in_git_repo .and. trim(mode) == 'git') then
                call git_tag_prompt(current_dir, repo_name)
            end if
        case(70, 102)  ! 'F' or 'f' - git fetch
            if (in_git_repo .and. trim(mode) == 'git') then
                call git_fetch_prompt(current_dir, repo_name)
            end if
        case(76, 108)  ! 'L' or 'l' - git pull
            if (in_git_repo .and. trim(mode) == 'git') then
                call git_pull_prompt(current_dir, repo_name)
            end if
        case(68, 100)  ! 'D' or 'd' - show git diff
            if (in_git_repo .and. trim(mode) == 'git' .and. .not. current_is_dir(selected)) then
                if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                    if (current_is_staged(selected) .or. current_is_unstaged(selected)) then
                        call show_git_diff_fullscreen(current_dir, current_files(selected), &
                                                      current_is_staged(selected), current_is_unstaged(selected))
                    end if
                end if
            end if
        case(18)  ! Alt+r - delete/remove with confirmation
            if (selection_count > 0) then
                ! Delete multiple selections
                call delete_multi_with_confirmation(current_dir, current_files, current_is_dir, &
                                                   is_selected, selection_count, current_count)
                ! Clear selections after delete
                call clear_all_selections(is_selected, selection_count, in_selection_mode)
            else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                call delete_with_confirmation(current_dir, current_files(selected), current_is_dir(selected))
            end if
        case(46)  ! '.' - toggle dotfiles visibility
            show_dotfiles = .not. show_dotfiles
            ! Reset selection to avoid going out of bounds
            selected = 1
            scroll_offset = 0
        case(126)  ! '~' - go to home directory
            call get_environment_variable("HOME", temp_dir)
            if (len_trim(temp_dir) > 0) then
                current_dir = temp_dir
                parent_dir = get_parent_path(current_dir)
                selected = 1
                scroll_offset = 0
                selection_anchor = -1
                call clear_all_selections(is_selected, selection_count, in_selection_mode)
                call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
            end if
        case(47)  ! '/' - go to root directory
            current_dir = "/"
            parent_dir = "/"
            selected = 1
            scroll_offset = 0
            selection_anchor = -1
            call clear_all_selections(is_selected, selection_count, in_selection_mode)
            call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
        case(42)  ! '*' - toggle favorite on current directory
            if (current_is_dir(selected) .and. trim(current_files(selected)) /= "." .and. &
                trim(current_files(selected)) /= "..") then
                ! Build full path for the directory
                temp_dir = join_path(current_dir, current_files(selected))

                ! Check if already favorited
                if (is_dir_favorited(temp_dir, favorite_dirs, favorite_count)) then
                    ! Remove from favorites
                    call toggle_favorite(temp_dir, favorite_dirs, favorite_count)
                    call save_favorites(favorite_dirs, favorite_count)
                else
                    ! Try to add
                    if (favorite_count < 10) then
                        ! Room available - add it
                        call toggle_favorite(temp_dir, favorite_dirs, favorite_count)
                        call save_favorites(favorite_dirs, favorite_count)
                    else
                        ! Full - prompt for replacement
                        call add_favorite_with_replacement(temp_dir, favorite_dirs, favorite_count)
                        call save_favorites(favorite_dirs, favorite_count)
                    end if
                end if
            end if
        case(56)  ! '8' - open favorites picker
            call open_favorites_picker(favorite_dirs, favorite_count, temp_dir)
            if (len_trim(temp_dir) > 0) then
                ! Navigate to selected favorite
                parent_dir = get_parent_path(temp_dir)
                current_dir = temp_dir
                selected = 1
                scroll_offset = 0
                selection_anchor = -1
                call clear_all_selections(is_selected, selection_count, in_selection_mode)
                call detect_git_repo(current_dir, in_git_repo, repo_name, branch_name)
            end if
        case(32)  ! Space - toggle selection on current item
            if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                if (is_selected(selected)) then
                    is_selected(selected) = .false.
                    selection_count = selection_count - 1
                else
                    is_selected(selected) = .true.
                    selection_count = selection_count + 1
                    in_selection_mode = .true.
                end if
                ! Clear the selection anchor when toggling individual items
                selection_anchor = -1
                ! Check if we have disjoint selections
                call check_disjoint_selection(is_selected, current_count, has_disjoint_selection)
            end if
        case(13)  ! Alt+m - enter move mode OR confirm move
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
        case(69, 101)  ! 'E' or 'e' - exit/clear multi-select mode
            if (selection_count > 0) then
                call clear_all_selections(is_selected, selection_count, in_selection_mode)
                selection_anchor = -1
            end if
        case(25)  ! Alt+y - yank/copy to clipboard
            if (selection_count > 0) then
                ! Copy multiple selections
                clipboard_count = 0
                do i = 1, current_count
                    if (is_selected(i) .and. trim(current_files(i)) /= "." .and. &
                        trim(current_files(i)) /= "..") then
                        clipboard_count = clipboard_count + 1
                        clipboard_paths(clipboard_count) = join_path(current_dir, current_files(i))
                        clipboard_names(clipboard_count) = current_files(i)
                    end if
                end do
                clipboard_is_cut = .false.
                has_clipboard = .true.
                ! Clear selections after copy
                call clear_all_selections(is_selected, selection_count, in_selection_mode)
            else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                ! Single item copy
                clipboard_count = 1
                clipboard_paths(1) = join_path(current_dir, current_files(selected))
                clipboard_names(1) = current_files(selected)
                clipboard_source_path = clipboard_paths(1)  ! For backward compatibility
                clipboard_source_name = clipboard_names(1)
                clipboard_is_cut = .false.
                has_clipboard = .true.
            end if
        case(24)  ! Alt+x - cut to clipboard
            if (selection_count > 0) then
                ! Cut multiple selections
                clipboard_count = 0
                do i = 1, current_count
                    if (is_selected(i) .and. trim(current_files(i)) /= "." .and. &
                        trim(current_files(i)) /= "..") then
                        clipboard_count = clipboard_count + 1
                        clipboard_paths(clipboard_count) = join_path(current_dir, current_files(i))
                        clipboard_names(clipboard_count) = current_files(i)
                    end if
                end do
                clipboard_is_cut = .true.
                has_clipboard = .true.
                ! Clear selections after cut
                call clear_all_selections(is_selected, selection_count, in_selection_mode)
            else if (trim(current_files(selected)) /= "." .and. trim(current_files(selected)) /= "..") then
                ! Single item cut
                clipboard_count = 1
                clipboard_paths(1) = join_path(current_dir, current_files(selected))
                clipboard_names(1) = current_files(selected)
                clipboard_source_path = clipboard_paths(1)  ! For backward compatibility
                clipboard_source_name = clipboard_names(1)
                clipboard_is_cut = .true.
                has_clipboard = .true.
            end if
        case(16)  ! Alt+p - paste from clipboard
            if (has_clipboard) then
                if (clipboard_count > 1) then
                    ! Paste multiple items
                    call execute_multi_paste(clipboard_paths, clipboard_names, clipboard_count, &
                                            clipboard_is_cut, current_dir, current_files(selected), &
                                            current_is_dir(selected))
                else
                    ! Single item paste (backward compatibility)
                    call execute_paste(clipboard_paths(1), clipboard_is_cut, current_dir, &
                                      current_files(selected), current_is_dir(selected))
                end if
                ! Clear clipboard after cut operation
                if (clipboard_is_cut) then
                    has_clipboard = .false.
                    clipboard_count = 0
                end if
            end if
        end select
    end do

    ! Cleanup
    call show_cursor()  ! Restore cursor visibility
    call exit_alt_screen()  ! Return to normal screen buffer
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

    subroutine execute_paste(source_path, is_cut, dest_dir, dest_name, dest_is_dir)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD, YELLOW
        character(len=*), intent(in) :: source_path, dest_dir, dest_name
        logical, intent(in) :: is_cut, dest_is_dir
        character(len=MAX_PATH*2) :: dest_path, cmd, final_dest, base_name, extension
        character(len=MAX_PATH*2) :: test_path
        character(len=10) :: suffix_str
        integer :: stat, suffix_num, ext_pos, name_len, ios

        ! Determine destination directory based on cursor position
        if (dest_is_dir) then
            if (trim(dest_name) == ".") then
                ! Paste into current directory
                dest_path = dest_dir
            else if (trim(dest_name) == "..") then
                ! Paste into parent directory
                dest_path = get_parent_path(dest_dir)
            else
                ! Paste into the selected directory
                dest_path = join_path(dest_dir, dest_name)
            end if
        else
            ! Cursor is on a file - paste into current directory (next to the file)
            dest_path = dest_dir
        end if

        ! Extract the source filename from source_path
        stat = index(source_path, "/", back=.true.)
        if (stat > 0) then
            base_name = source_path(stat+1:)
        else
            base_name = source_path
        end if

        ! Build initial destination (directory + filename)
        final_dest = join_path(dest_path, base_name)

        ! Check if destination exists and find available suffix if needed
        call execute_command_line("test -e '" // trim(final_dest) // "'", exitstat=stat, wait=.true.)
        if (stat == 0) then
            ! Destination exists - find next available suffix (for both copy and cut)
            ! Split filename into name and extension
            ext_pos = index(base_name, ".", back=.true.)
            if (ext_pos > 1) then
                ! Has extension
                extension = base_name(ext_pos:)
                name_len = ext_pos - 1
            else
                ! No extension
                extension = ""
                name_len = len_trim(base_name)
            end if

            ! Find next available suffix number
            suffix_num = 1
            do while (suffix_num < 1000)  ! Safety limit
                ! Build the test path with suffix using concatenation
                write(suffix_str, '(i0)') suffix_num

                if (len_trim(extension) > 0) then
                    ! With extension: filename-N.ext
                    test_path = trim(dest_path) // "/" // base_name(1:name_len) // "-" // &
                                trim(suffix_str) // trim(extension)
                else
                    ! Without extension: filename-N
                    test_path = trim(dest_path) // "/" // trim(base_name) // "-" // trim(suffix_str)
                end if

                ! Check if this suffixed name exists
                call execute_command_line("test -e '" // trim(test_path) // "'", exitstat=stat, wait=.true.)
                if (stat /= 0) then
                    ! This name is available!
                    final_dest = test_path
                    exit
                end if
                suffix_num = suffix_num + 1
            end do
        end if

        ! Execute copy or move command
        if (is_cut) then
            ! Cut = move
            cmd = "mv '" // trim(source_path) // "' '" // trim(final_dest) // "'"
        else
            ! Copy recursively (works for both files and directories)
            cmd = "cp -r '" // trim(source_path) // "' '" // trim(final_dest) // "'"
        end if
        call execute_command_line(trim(cmd), exitstat=stat, wait=.true.)

        ! Show result briefly
        write(output_unit, '(a)', advance='no') CLEAR
        if (is_cut) then
            write(output_unit, '(a)') BOLD // "Cut Result" // RESET
        else
            write(output_unit, '(a)') BOLD // "Copy Result" // RESET
        end if
        write(output_unit, *)
        if (stat == 0) then
            if (is_cut) then
                write(output_unit, '(a)') GREEN // "✓ Cut and pasted successfully!" // RESET
            else
                write(output_unit, '(a)') GREEN // "✓ Copied successfully!" // RESET
            end if
            write(output_unit, '(a)') "  From: " // trim(source_path)
            write(output_unit, '(a)') "  To:   " // trim(final_dest)
        else
            if (is_cut) then
                write(output_unit, '(a)') RED // "✗ Cut failed" // RESET
            else
                write(output_unit, '(a)') RED // "✗ Copy failed" // RESET
            end if
            write(output_unit, '(a)') "  (destination may already exist or be invalid)"
        end if
        write(output_unit, *)

        ! Brief pause to let user see the result
        call sleep(2)
    end subroutine execute_paste

    subroutine delete_with_confirmation(dir, filename, is_dir)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD, YELLOW
        character(len=*), intent(in) :: dir, filename
        logical, intent(in) :: is_dir
        character(len=MAX_PATH*2) :: full_path, rm_cmd
        character(len=1) :: response
        integer :: stat, ios

        ! Build full path
        full_path = join_path(dir, filename)

        ! Clear screen and show confirmation prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)') BOLD // "Delete Confirmation" // RESET
        write(output_unit, *)
        if (is_dir) then
            write(output_unit, '(a)') YELLOW // "WARNING: You are about to delete a directory!" // RESET
            write(output_unit, '(a)') "Directory: " // trim(filename)
        else
            write(output_unit, '(a)') "File: " // trim(filename)
        end if
        write(output_unit, '(a)') "Path: " // trim(full_path)
        write(output_unit, *)
        write(output_unit, '(a)', advance='no') RED // "Are you sure? (y/N): " // RESET

        ! Read single character immediately (no need to wait for Enter)
        read(*, '(a1)', advance='no', iostat=ios) response

        if (ios == 0 .and. (response == 'y' .or. response == 'Y')) then
            ! User confirmed - proceed with deletion
            if (is_dir) then
                ! Delete directory recursively
                rm_cmd = "rm -rf '" // trim(full_path) // "'"
            else
                ! Delete file
                rm_cmd = "rm -f '" // trim(full_path) // "'"
            end if

            call execute_command_line(trim(rm_cmd), exitstat=stat, wait=.true.)

            ! Show result
            write(output_unit, *)
            write(output_unit, *)
            if (stat == 0) then
                write(output_unit, '(a)') GREEN // "✓ Deleted successfully!" // RESET
            else
                write(output_unit, '(a)') RED // "✗ Delete failed" // RESET
            end if
            write(output_unit, *)
            write(output_unit, '(a)') "Press any key to continue..."

            ! Wait for keypress
            read(*, '(a1)', advance='no', iostat=ios) response
        else
            ! User cancelled
            write(output_unit, *)
            write(output_unit, *)
            write(output_unit, '(a)') "Delete cancelled."
            write(output_unit, *)
            write(output_unit, '(a)') "Press any key to continue..."

            ! Wait for keypress
            read(*, '(a1)', advance='no', iostat=ios) response
        end if
    end subroutine delete_with_confirmation

    subroutine clear_all_selections(is_selected, selection_count, in_selection_mode)
        logical, dimension(*), intent(inout) :: is_selected
        integer, intent(inout) :: selection_count
        logical, intent(inout) :: in_selection_mode
        integer :: i

        ! Clear all selections
        do i = 1, MAX_FILES
            is_selected(i) = .false.
        end do
        selection_count = 0
        in_selection_mode = .false.
    end subroutine clear_all_selections

    subroutine check_disjoint_selection(is_selected, count, has_disjoint)
        logical, dimension(*), intent(in) :: is_selected
        integer, intent(in) :: count
        logical, intent(out) :: has_disjoint
        integer :: i, first_selected, last_selected

        has_disjoint = .false.
        first_selected = -1
        last_selected = -1

        ! Find first and last selected items
        do i = 1, count
            if (is_selected(i)) then
                if (first_selected == -1) first_selected = i
                last_selected = i
            end if
        end do

        ! If we have a range, check if all items in range are selected
        if (first_selected > 0 .and. last_selected > first_selected) then
            do i = first_selected + 1, last_selected - 1
                if (.not. is_selected(i)) then
                    has_disjoint = .true.
                    exit
                end if
            end do
        end if
    end subroutine check_disjoint_selection

    subroutine select_range(is_selected, selection_count, anchor, cursor, files, count)
        logical, dimension(*), intent(inout) :: is_selected
        integer, intent(inout) :: selection_count
        integer, intent(in) :: anchor, cursor, count
        character(len=*), dimension(*), intent(in) :: files
        integer :: i, range_start, range_end

        ! Determine range boundaries
        range_start = min(anchor, cursor)
        range_end = max(anchor, cursor)

        ! Clear all selections first
        do i = 1, count
            is_selected(i) = .false.
        end do

        ! Select the range, skipping "." and ".."
        selection_count = 0
        do i = range_start, range_end
            if (i >= 1 .and. i <= count) then
                ! Skip special directories "." and ".."
                if (trim(files(i)) /= "." .and. trim(files(i)) /= "..") then
                    is_selected(i) = .true.
                    selection_count = selection_count + 1
                end if
            end if
        end do
    end subroutine select_range

    subroutine delete_multi_with_confirmation(dir, files, is_dir, is_selected, selection_count, count)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD, YELLOW
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir, is_selected
        integer, intent(in) :: selection_count, count
        character(len=MAX_PATH*2) :: full_path, rm_cmd
        character(len=1) :: response
        integer :: stat, ios, i, deleted_count

        ! Clear screen and show confirmation prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)') BOLD // "Delete Multiple Items" // RESET
        write(output_unit, *)
        write(output_unit, '(a)') YELLOW // "WARNING: You are about to delete " // &
                                  trim(adjustl(itoa(selection_count))) // " items!" // RESET
        write(output_unit, *)
        write(output_unit, '(a)') "Selected items:"

        ! List selected items (up to 10)
        i = 0
        do stat = 1, count
            if (is_selected(stat)) then
                i = i + 1
                if (i <= 10) then
                    if (is_dir(stat)) then
                        write(output_unit, '(a)') "  [DIR]  " // trim(files(stat))
                    else
                        write(output_unit, '(a)') "  [FILE] " // trim(files(stat))
                    end if
                else if (i == 11) then
                    write(output_unit, '(a)') "  ... and " // &
                        trim(adjustl(itoa(selection_count - 10))) // " more"
                    exit
                end if
            end if
        end do

        write(output_unit, *)
        write(output_unit, '(a)', advance='no') RED // "Delete all selected items? (y/N): " // RESET

        ! Read single character immediately
        read(*, '(a1)', advance='no', iostat=ios) response

        if (ios == 0 .and. (response == 'y' .or. response == 'Y')) then
            ! User confirmed - proceed with deletion
            deleted_count = 0

            do i = 1, count
                if (is_selected(i) .and. trim(files(i)) /= "." .and. trim(files(i)) /= "..") then
                    full_path = join_path(dir, files(i))

                    if (is_dir(i)) then
                        rm_cmd = "rm -rf '" // trim(full_path) // "'"
                    else
                        rm_cmd = "rm -f '" // trim(full_path) // "'"
                    end if

                    call execute_command_line(trim(rm_cmd), exitstat=stat, wait=.true.)
                    if (stat == 0) deleted_count = deleted_count + 1
                end if
            end do

            ! Show result
            write(output_unit, *)
            write(output_unit, *)
            if (deleted_count == selection_count) then
                write(output_unit, '(a)') GREEN // "✓ All items deleted successfully!" // RESET
            else if (deleted_count > 0) then
                write(output_unit, '(a)') YELLOW // "⚠ Deleted " // &
                    trim(adjustl(itoa(deleted_count))) // " of " // &
                    trim(adjustl(itoa(selection_count))) // " items" // RESET
            else
                write(output_unit, '(a)') RED // "✗ Delete failed" // RESET
            end if
            write(output_unit, *)
            write(output_unit, '(a)') "Press any key to continue..."

            ! Wait for keypress
            read(*, '(a1)', advance='no', iostat=ios) response
        else
            ! User cancelled
            write(output_unit, *)
            write(output_unit, *)
            write(output_unit, '(a)') "Delete cancelled."
            write(output_unit, *)
            write(output_unit, '(a)') "Press any key to continue..."

            ! Wait for keypress
            read(*, '(a1)', advance='no', iostat=ios) response
        end if
    end subroutine delete_multi_with_confirmation

    function itoa(n) result(str)
        integer, intent(in) :: n
        character(len=10) :: str
        write(str, '(i0)') n
    end function itoa

    subroutine execute_multi_paste(paths, names, count, is_cut, dest_dir, dest_name, dest_is_dir)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD, YELLOW
        character(len=*), dimension(*), intent(in) :: paths, names
        integer, intent(in) :: count
        logical, intent(in) :: is_cut, dest_is_dir
        character(len=*), intent(in) :: dest_dir, dest_name
        character(len=MAX_PATH*2) :: dest_path, cmd, final_dest
        integer :: i, stat, success_count
        character(len=1) :: response

        ! Determine destination directory
        if (dest_is_dir) then
            if (trim(dest_name) == ".") then
                dest_path = dest_dir
            else if (trim(dest_name) == "..") then
                dest_path = get_parent_path(dest_dir)
            else
                dest_path = join_path(dest_dir, dest_name)
            end if
        else
            dest_path = dest_dir
        end if

        ! Show operation preview
        write(output_unit, '(a)', advance='no') CLEAR
        if (is_cut) then
            write(output_unit, '(a)') BOLD // "Multi-Cut Operation" // RESET
        else
            write(output_unit, '(a)') BOLD // "Multi-Copy Operation" // RESET
        end if
        write(output_unit, *)
        write(output_unit, '(a)') "Pasting " // trim(adjustl(itoa(count))) // " items to:"
        write(output_unit, '(a)') "  " // trim(dest_path)
        write(output_unit, *)

        success_count = 0

        ! Process each item
        do i = 1, count
            ! Generate unique destination name if needed
            call get_unique_dest_name(dest_path, names(i), final_dest)

            ! Execute operation
            if (is_cut) then
                cmd = "mv '" // trim(paths(i)) // "' '" // trim(final_dest) // "'"
            else
                cmd = "cp -r '" // trim(paths(i)) // "' '" // trim(final_dest) // "'"
            end if

            call execute_command_line(trim(cmd), exitstat=stat, wait=.true.)

            if (stat == 0) then
                success_count = success_count + 1
                write(output_unit, '(a)') GREEN // "  ✓ " // RESET // trim(names(i))
            else
                write(output_unit, '(a)') RED // "  ✗ " // RESET // trim(names(i))
            end if
        end do

        ! Show summary
        write(output_unit, *)
        if (success_count == count) then
            write(output_unit, '(a)') GREEN // "✓ All items processed successfully!" // RESET
        else if (success_count > 0) then
            write(output_unit, '(a)') YELLOW // "⚠ Processed " // &
                trim(adjustl(itoa(success_count))) // " of " // &
                trim(adjustl(itoa(count))) // " items" // RESET
        else
            write(output_unit, '(a)') RED // "✗ Operation failed" // RESET
        end if
        write(output_unit, *)
        write(output_unit, '(a)') "Press any key to continue..."

        ! Wait for keypress
        read(*, '(a1)', advance='no', iostat=stat) response
    end subroutine execute_multi_paste

    subroutine get_unique_dest_name(dest_dir, base_name, unique_name)
            character(len=*), intent(in) :: dest_dir, base_name
            character(len=*), intent(out) :: unique_name
            character(len=MAX_PATH*2) :: test_path
            character(len=256) :: name_part, extension
            character(len=10) :: suffix_str
            integer :: ext_pos, suffix_num, stat

            ! Initial destination
            unique_name = join_path(dest_dir, base_name)

            ! Check if exists
            call execute_command_line("test -e '" // trim(unique_name) // "'", exitstat=stat, wait=.true.)
            if (stat /= 0) return  ! Doesn't exist, we're good

            ! Split name and extension
            ext_pos = index(base_name, ".", back=.true.)
            if (ext_pos > 1) then
                name_part = base_name(1:ext_pos-1)
                extension = base_name(ext_pos:)
            else
                name_part = base_name
                extension = ""
            end if

            ! Find available suffix
            do suffix_num = 1, 999
                write(suffix_str, '(i0)') suffix_num
                if (len_trim(extension) > 0) then
                    test_path = trim(dest_dir) // "/" // trim(name_part) // "-" // &
                               trim(suffix_str) // trim(extension)
                else
                    test_path = trim(dest_dir) // "/" // trim(name_part) // "-" // &
                               trim(suffix_str)
                end if

                call execute_command_line("test -e '" // trim(test_path) // "'", exitstat=stat, wait=.true.)
                if (stat /= 0) then
                    unique_name = test_path
                    exit
                end if
            end do
    end subroutine get_unique_dest_name

    subroutine load_favorites(favs, count)
        character(len=MAX_PATH), dimension(10), intent(out) :: favs
        integer, intent(out) :: count
        character(len=MAX_PATH) :: favorites_file
        integer :: unit, ios

        count = 0
        call get_environment_variable("HOME", favorites_file)
        favorites_file = trim(favorites_file) // "/.fortress_favorites"

        open(newunit=unit, file=favorites_file, status='old', action='read', iostat=ios)
        if (ios /= 0) return  ! File doesn't exist yet

        do while (count < 10)
            read(unit, '(a)', iostat=ios) favs(count + 1)
            if (ios /= 0) exit
            if (len_trim(favs(count + 1)) > 0) count = count + 1
        end do

        close(unit)
    end subroutine load_favorites

    subroutine save_favorites(favs, count)
        character(len=MAX_PATH), dimension(10), intent(in) :: favs
        integer, intent(in) :: count
        character(len=MAX_PATH) :: favorites_file
        integer :: unit, ios, i

        call get_environment_variable("HOME", favorites_file)
        favorites_file = trim(favorites_file) // "/.fortress_favorites"

        open(newunit=unit, file=favorites_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) return

        do i = 1, count
            write(unit, '(a)') trim(favs(i))
        end do

        close(unit)
    end subroutine save_favorites

    function is_dir_favorited(dir_path, favs, count) result(is_fav)
        character(len=*), intent(in) :: dir_path
        character(len=MAX_PATH), dimension(10), intent(in) :: favs
        integer, intent(in) :: count
        logical :: is_fav
        integer :: i

        is_fav = .false.
        do i = 1, count
            if (trim(favs(i)) == trim(dir_path)) then
                is_fav = .true.
                return
            end if
        end do
    end function is_dir_favorited

    subroutine toggle_favorite(dir_path, favs, count)
        character(len=*), intent(in) :: dir_path
        character(len=MAX_PATH), dimension(10), intent(inout) :: favs
        integer, intent(inout) :: count
        integer :: i, j
        logical :: found

        ! Check if already favorited
        found = .false.
        do i = 1, count
            if (trim(favs(i)) == trim(dir_path)) then
                ! Found - remove it
                found = .true.
                ! Shift remaining favorites down
                do j = i, count - 1
                    favs(j) = favs(j + 1)
                end do
                favs(count) = ""
                count = count - 1
                exit
            end if
        end do

        if (.not. found) then
            ! Not found - add it (if we have space)
            if (count < 10) then
                count = count + 1
                favs(count) = dir_path
            end if
        end if
    end subroutine toggle_favorite

    subroutine add_favorite_with_replacement(dir_path, favs, count)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD
        character(len=*), intent(in) :: dir_path
        character(len=MAX_PATH), dimension(10), intent(inout) :: favs
        integer, intent(in) :: count
        character(len=MAX_PATH) :: temp_file, selected_fav
        integer :: unit, ios, stat, i

        ! Clear screen and show prompt
        write(output_unit, '(a)', advance='no') CLEAR
        write(output_unit, '(a)') BOLD // "Favorites Full (10/10)" // RESET
        write(output_unit, *)
        write(output_unit, '(a)') "Select a favorite to replace:"
        write(output_unit, *)

        ! Restore terminal for fzf
        call execute_command_line("stty sane 2>/dev/null")

        ! Create temp file with current favorites
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_fav_temp"

        open(newunit=unit, file=temp_file, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            do i = 1, count
                write(unit, '(a)') trim(favs(i))
            end do
            close(unit)
        end if

        ! Use fzf to select which favorite to replace
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_fav_select"
        call execute_command_line("cat ~/.fortress_fav_temp | fzf --height=10 --prompt='Replace: ' > " // &
                                 trim(temp_file) // " 2>/dev/null", exitstat=stat, wait=.true.)

        if (stat == 0) then
            ! Read selected favorite
            open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
            if (ios == 0) then
                read(unit, '(a)', iostat=ios) selected_fav
                close(unit)

                if (ios == 0 .and. len_trim(selected_fav) > 0) then
                    ! Replace the selected favorite with the new one
                    do i = 1, count
                        if (trim(favs(i)) == trim(selected_fav)) then
                            favs(i) = dir_path
                            write(output_unit, '(a)') GREEN // "✓ Replaced: " // trim(selected_fav) // RESET
                            write(output_unit, '(a)') "     With: " // trim(dir_path)
                            call execute_command_line("sleep 1")
                            exit
                        end if
                    end do
                end if
            end if
        else
            write(output_unit, '(a)') RED // "Cancelled." // RESET
            call execute_command_line("sleep 1")
        end if

        ! Cleanup temp files
        call execute_command_line("rm -f ~/.fortress_fav_temp ~/.fortress_fav_select 2>/dev/null")

        ! Re-enable raw mode
        call setup_raw_mode()
    end subroutine add_favorite_with_replacement

    subroutine mark_favorites_in_lists(current_dir, files, count, is_dir, favs, fav_count, is_fav)
        character(len=*), intent(in) :: current_dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        logical, dimension(*), intent(in) :: is_dir
        character(len=MAX_PATH), dimension(10), intent(in) :: favs
        integer, intent(in) :: fav_count
        logical, dimension(*), intent(out) :: is_fav
        character(len=MAX_PATH) :: full_path
        integer :: i

        ! Initialize all to false
        do i = 1, count
            is_fav(i) = .false.
        end do

        ! Mark directories that are in favorites
        do i = 1, count
            if (is_dir(i) .and. trim(files(i)) /= "." .and. trim(files(i)) /= "..") then
                ! Build full path and check if favorited
                full_path = join_path(current_dir, files(i))
                is_fav(i) = is_dir_favorited(full_path, favs, fav_count)
            end if
        end do
    end subroutine mark_favorites_in_lists

    subroutine open_favorites_picker(favs, count, selected_dir)
        use iso_fortran_env, only: output_unit
        use terminal_control, only: CLEAR, GREEN, RED, RESET, BOLD
        character(len=MAX_PATH), dimension(10), intent(in) :: favs
        integer, intent(in) :: count
        character(len=MAX_PATH), intent(out) :: selected_dir
        character(len=MAX_PATH) :: temp_file
        integer :: unit, ios, stat, i

        selected_dir = ""

        if (count == 0) then
            ! No favorites yet
            write(output_unit, '(a)', advance='no') CLEAR
            write(output_unit, '(a)') RED // "No favorites yet!" // RESET
            write(output_unit, *)
            write(output_unit, '(a)') "Press '*' on a directory to add it to favorites."
            call execute_command_line("sleep 2")
            return
        end if

        ! Restore terminal for fzf
        call execute_command_line("stty sane 2>/dev/null")

        ! Create temp file with favorites
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_fav_picker"

        open(newunit=unit, file=temp_file, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            do i = 1, count
                write(unit, '(a)') trim(favs(i))
            end do
            close(unit)
        end if

        ! Use fzf to select favorite
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_fav_selected"
        call execute_command_line("cat ~/.fortress_fav_picker | fzf --height=10 --prompt='Jump to: ' > " // &
                                 trim(temp_file) // " 2>/dev/null", exitstat=stat, wait=.true.)

        if (stat == 0) then
            ! Read selected directory
            open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
            if (ios == 0) then
                read(unit, '(a)', iostat=ios) selected_dir
                close(unit)
            end if
        end if

        ! Cleanup temp files
        call execute_command_line("rm -f ~/.fortress_fav_picker ~/.fortress_fav_selected 2>/dev/null")

        ! Re-enable raw mode
        call setup_raw_mode()
    end subroutine open_favorites_picker

    subroutine fuzzy_jump(files, count, pattern, selected)
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        character(len=*), intent(in) :: pattern
        integer, intent(inout) :: selected
        integer :: i, score, best_score, best_idx
        character(len=256) :: pattern_lower, filename_lower

        if (len_trim(pattern) == 0) return

        best_score = -1
        best_idx = selected

        ! Convert pattern to lowercase
        pattern_lower = pattern
        call to_lowercase(pattern_lower)

        ! Search for best match
        do i = 1, count
            ! Skip "." and ".."
            if (trim(files(i)) == "." .or. trim(files(i)) == "..") cycle

            ! Convert filename to lowercase
            filename_lower = files(i)
            call to_lowercase(filename_lower)

            ! Get fuzzy match score
            score = fuzzy_score(pattern_lower, filename_lower)

            if (score > best_score) then
                best_score = score
                best_idx = i
            end if
        end do

        ! Jump to best match if we found something
        if (best_score >= 0) then
            selected = best_idx
        end if
    end subroutine fuzzy_jump

    function fuzzy_score(pattern, text) result(score)
        character(len=*), intent(in) :: pattern, text
        integer :: score
        integer :: i, j, pat_len, text_len, consecutive
        logical :: match_found
        character(len=256) :: pattern_trim, text_trim

        pattern_trim = trim(pattern)
        text_trim = trim(text)
        pat_len = len_trim(pattern_trim)
        text_len = len_trim(text_trim)

        if (pat_len == 0) then
            score = 0
            return
        end if

        ! Exact match (highest priority)
        if (pattern_trim == text_trim) then
            score = 10000
            return
        end if

        ! Prefix match (very high priority)
        if (text_len >= pat_len) then
            if (text_trim(1:pat_len) == pattern_trim) then
                score = 5000
                return
            end if
        end if

        ! Fuzzy match with scoring
        score = 0
        consecutive = 0
        j = 1

        do i = 1, pat_len
            match_found = .false.
            do while (j <= text_len)
                if (pattern_trim(i:i) == text_trim(j:j)) then
                    ! Base score for character match
                    score = score + 100

                    ! Bonus for consecutive characters
                    if (consecutive > 0) then
                        score = score + 50
                    end if
                    consecutive = consecutive + 1

                    ! Bonus for match at start
                    if (j == i) then
                        score = score + 200
                    end if

                    ! Bonus for word boundary (after /, -, _, .)
                    if (j > 1) then
                        if (text_trim(j-1:j-1) == '/' .or. text_trim(j-1:j-1) == '-' .or. &
                            text_trim(j-1:j-1) == '_' .or. text_trim(j-1:j-1) == '.') then
                            score = score + 150
                        end if
                    end if

                    j = j + 1
                    match_found = .true.
                    exit
                else
                    consecutive = 0
                    j = j + 1
                end if
            end do

            ! If character not found, no match
            if (.not. match_found) then
                score = -1
                return
            end if
        end do

        ! Penalty for length (prefer shorter matches)
        score = score - (text_len - pat_len)
    end function fuzzy_score

    subroutine to_lowercase(str)
        character(len=*), intent(inout) :: str
        integer :: i, char_code

        do i = 1, len_trim(str)
            char_code = ichar(str(i:i))
            if (char_code >= ichar('A') .and. char_code <= ichar('Z')) then
                str(i:i) = achar(char_code + 32)
            end if
        end do
    end subroutine to_lowercase

end program fortress
