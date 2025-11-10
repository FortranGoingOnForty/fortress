module ui_display
    use iso_fortran_env, only: output_unit
    use terminal_control
    use git_ops, only: write_git_indicators
    use filesystem_ops, only: MAX_PATH, MAX_FILES
    implicit none
    private

    public :: draw_interface, get_file_color

contains

    subroutine draw_interface(r, c, current_dir, current_files, current_is_dir, current_is_exec, &
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
        integer, intent(in) :: r, c, current_count, parent_count, selected, parent_selected
        integer, intent(in) :: scroll_offset, parent_scroll_offset
        character(len=*), intent(in) :: current_dir, repo_name, branch_name, mode
        character(len=*), dimension(*), intent(in) :: current_files, parent_files
        logical, dimension(*), intent(in) :: current_is_dir, parent_is_dir
        logical, dimension(*), intent(in) :: current_is_exec, parent_is_exec
        logical, dimension(*), intent(in) :: current_is_staged, current_is_unstaged, current_is_untracked
        logical, dimension(*), intent(in) :: current_has_incoming
        logical, intent(in) :: in_git_repo, move_mode
        character(len=*), intent(in) :: move_source_name
        integer, intent(in) :: move_dest_selected
        logical, intent(in) :: has_clipboard, clipboard_is_cut
        character(len=*), intent(in) :: clipboard_source_name
        integer, intent(in) :: clipboard_count
        logical, dimension(*), intent(in) :: is_selected
        integer, intent(in) :: selection_count
        logical, dimension(*), intent(in) :: current_is_favorite, parent_is_favorite
        character(len=*), intent(in) :: search_buffer
        integer, intent(in) :: search_length
        logical, intent(in) :: in_rename_mode
        character(len=*), intent(in) :: rename_buffer
        integer, intent(in) :: rename_cursor_pos
        integer :: left_w, i, parent_idx, current_idx, vis_h, display_len
        character(len=256) :: fname
        character(len=20) :: color_code

        left_w = c * 3 / 10
        vis_h = r - 2  ! Visible height (header + footer)

        ! Header
        if (in_rename_mode) then
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                     " | " // YELLOW // "RENAME MODE" // RESET
        else if (move_mode) then
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                     " | " // RED // "MOVE: " // trim(move_source_name) // RESET
        else if (selection_count > 0) then
            ! Show selection count
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                     " | " // BLUE // trim(adjustl(itoa(selection_count))) // " selected" // RESET
        else if (has_clipboard) then
            if (clipboard_count > 1) then
                ! Multiple items in clipboard
                if (clipboard_is_cut) then
                    write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                             " | " // YELLOW // "CUT: " // trim(adjustl(itoa(clipboard_count))) // " items" // RESET
                else
                    write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                             " | " // GREEN // "COPY: " // trim(adjustl(itoa(clipboard_count))) // " items" // RESET
                end if
            else
                ! Single item in clipboard
                if (clipboard_is_cut) then
                    write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                             " | " // YELLOW // "CUT: " // trim(clipboard_source_name) // RESET
                else
                    write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                             " | " // GREEN // "COPY: " // trim(clipboard_source_name) // RESET
                end if
            end if
        else if (in_git_repo .and. trim(mode) == 'git') then
            ! Show git mode indicator
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                     " | " // CYAN // trim(repo_name) // ":" // YELLOW // trim(branch_name) // " " // &
                                     YELLOW // BOLD // "[ GIT MODE ]" // RESET
        else if (in_git_repo) then
            ! Show repo info without mode indicator
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                     " | " // CYAN // trim(repo_name) // ":" // YELLOW // trim(branch_name) // RESET
        else
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir)
        end if

        ! Files (render based on scroll offsets)
        do i = 1, vis_h
            parent_idx = i + parent_scroll_offset
            current_idx = i + scroll_offset

            ! Parent pane
            if (parent_idx >= 1 .and. parent_idx <= parent_count) then
                fname = parent_files(parent_idx)

                ! Track if this item has a star (for visual width adjustment)
                display_len = 0
                if (parent_is_favorite(parent_idx)) then
                    fname = "★ " // trim(fname)
                    display_len = 1  ! Star takes 2 visual columns, so add 1 extra
                end if

                if (parent_is_dir(parent_idx) .and. parent_files(parent_idx) /= "." .and. parent_files(parent_idx) /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for parent file
                color_code = get_file_color(parent_files(parent_idx), parent_is_dir(parent_idx), parent_is_exec(parent_idx))

                ! Calculate visual width: string length + extra for wide char
                display_len = min(len_trim(fname) + display_len, left_w)

                if (parent_idx == parent_selected) then
                    write(output_unit, '(a)', advance='no') DIM // BOLD // trim(color_code) // &
                        fname(1:min(len_trim(fname), left_w)) // RESET
                else
                    write(output_unit, '(a)', advance='no') DIM // trim(color_code) // &
                        fname(1:min(len_trim(fname), left_w)) // RESET
                end if
                write(output_unit, '(a)', advance='no') repeat(" ", max(0, left_w - display_len))
            else
                write(output_unit, '(a)', advance='no') repeat(" ", left_w)
            end if

            ! Separator
            write(output_unit, '(a)', advance='no') " │ "

            ! Current pane
            if (current_idx >= 1 .and. current_idx <= current_count) then
                fname = current_files(current_idx)

                ! Track if this item has a star (for visual width - star takes 2 columns)
                display_len = 0
                if (current_is_favorite(current_idx)) then
                    fname = "★ " // trim(fname)
                    display_len = 1  ! Add 1 to account for star being 2 visual columns
                end if

                if (current_is_dir(current_idx) .and. current_files(current_idx) /= "." .and. current_files(current_idx) /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Store the visual display length for this line (used by git indicators)
                display_len = len_trim(fname) + display_len

                ! Get color for current file
                color_code = get_file_color(current_files(current_idx), current_is_dir(current_idx), current_is_exec(current_idx))

                ! Check if this file is cut to clipboard (show in dark red)
                ! Only highlight for single-item cuts (multi-cuts shown in header)
                if (has_clipboard .and. clipboard_is_cut .and. clipboard_count == 1 .and. &
                    trim(current_files(current_idx)) == trim(clipboard_source_name)) then
                    ! File is cut - show in dark red (dimmed red)
                    write(output_unit, '(a)', advance='no') DIM // RED // trim(fname)
                    if (in_git_repo) then
                        call write_git_indicators(current_is_staged(current_idx), &
                                                  current_is_unstaged(current_idx), &
                                                  current_is_untracked(current_idx), &
                                                  current_has_incoming(current_idx), .false.)
                    end if
                    write(output_unit, '(a)') RESET
                ! Move mode: show source in red, destination in white
                else if (move_mode .and. trim(current_files(current_idx)) == trim(move_source_name)) then
                    ! Source file - show in RED
                    write(output_unit, '(a)', advance='no') RED // BOLD // trim(fname) // RESET
                    write(output_unit, '(a)') ""
                else if (move_mode .and. current_idx == move_dest_selected) then
                    ! Destination cursor - show with white background
                    write(output_unit, '(a)', advance='no') REVERSE // WHITE // trim(fname)
                    if (in_git_repo) then
                        call write_git_indicators(current_is_staged(current_idx), &
                                                  current_is_unstaged(current_idx), &
                                                  current_is_untracked(current_idx), &
                                                  current_has_incoming(current_idx), .true.)
                    end if
                    write(output_unit, '(a)') RESET
                else if (current_idx == selected .and. .not. move_mode) then
                    ! Normal selection cursor (not in move mode)
                    ! Check if in rename mode - show editable buffer with cursor
                    if (in_rename_mode) then
                        ! Show rename buffer with block cursor (█) at cursor position
                        if (current_is_favorite(current_idx)) then
                            write(output_unit, '(a)', advance='no') REVERSE // trim(color_code) // "★ "
                        else
                            write(output_unit, '(a)', advance='no') REVERSE // trim(color_code)
                        end if

                        if (rename_cursor_pos == len_trim(rename_buffer)) then
                            ! Cursor at end
                            write(output_unit, '(a)', advance='no') trim(rename_buffer) // '█'
                        else if (rename_cursor_pos == 0) then
                            ! Cursor at beginning
                            write(output_unit, '(a)', advance='no') '█' // trim(rename_buffer)
                        else
                            ! Cursor in middle
                            write(output_unit, '(a)', advance='no') rename_buffer(1:rename_cursor_pos) // '█' // &
                                   rename_buffer(rename_cursor_pos+1:len_trim(rename_buffer))
                        end if
                        if (current_is_dir(current_idx) .and. current_files(current_idx) /= "." .and. &
                            current_files(current_idx) /= "..") then
                            write(output_unit, '(a)', advance='no') "/"
                        end if
                        write(output_unit, '(a)') RESET
                    ! Not in rename mode - normal rendering
                    else if (has_clipboard .and. clipboard_is_cut .and. clipboard_count == 1 .and. &
                        trim(current_files(current_idx)) == trim(clipboard_source_name)) then
                        ! If file is cut, show selected with red background
                        write(output_unit, '(a)', advance='no') REVERSE // RED // trim(fname)
                        ! Add git indicators if in repo
                        if (in_git_repo) then
                            call write_git_indicators(current_is_staged(current_idx), &
                                                      current_is_unstaged(current_idx), &
                                                      current_is_untracked(current_idx), &
                                                      current_has_incoming(current_idx), .true.)
                        end if
                        write(output_unit, '(a)') RESET
                    else
                        write(output_unit, '(a)', advance='no') REVERSE // trim(color_code) // trim(fname)
                        ! Add git indicators if in repo
                        if (in_git_repo) then
                            call write_git_indicators(current_is_staged(current_idx), &
                                                      current_is_unstaged(current_idx), &
                                                      current_is_untracked(current_idx), &
                                                      current_has_incoming(current_idx), .true.)
                        end if
                        write(output_unit, '(a)') RESET
                    end if
                else if (is_selected(current_idx)) then
                    ! Multi-selected item (not the cursor) - show with cyan background
                    write(output_unit, '(a)', advance='no') REVERSE // trim(color_code) // trim(fname)
                    ! Add git indicators if in repo
                    if (in_git_repo) then
                        call write_git_indicators(current_is_staged(current_idx), &
                                                  current_is_unstaged(current_idx), &
                                                  current_is_untracked(current_idx), &
                                                  current_has_incoming(current_idx), .true.)
                    end if
                    write(output_unit, '(a)') RESET
                else
                    ! Normal rendering
                    write(output_unit, '(a)', advance='no') trim(color_code) // trim(fname)
                    ! Add git indicators if in repo
                    if (in_git_repo) then
                        call write_git_indicators(current_is_staged(current_idx), &
                                                  current_is_unstaged(current_idx), &
                                                  current_is_untracked(current_idx), &
                                                  current_has_incoming(current_idx), .false.)
                    end if
                    write(output_unit, '(a)') RESET
                end if
            else
                write(output_unit, *)
            end if
        end do

        ! Footer
        if (in_rename_mode) then
            write(output_unit, '(a)') YELLOW // "RENAME MODE: " // RESET // &
                                     DIM // "Type to edit | Enter:confirm ESC:cancel" // RESET
        else if (move_mode) then
            write(output_unit, '(a)') RED // "MOVE MODE: " // RESET // &
                                     DIM // "↑↓:next/prev dir →:enter dir ←:parent ~:home /:root alt-m:move here q:cancel" // RESET
        else if (selection_count > 0) then
            ! Selection mode footer - show multi-select help
            write(output_unit, '(a)') BLUE // "MULTI-SELECT: " // RESET // &
                                     DIM // "Space:toggle Shift+↑↓:block e:exit | alt-y:copy alt-x:cut alt-p:paste alt-r:delete | " // &
                                     "→:enter ←:back ~:home /:root alt-c:cd ctrl-q:quit" // RESET
        else if (in_git_repo .and. trim(mode) == 'git') then
            ! Git mode footer - show git operations in yellow
            write(output_unit, '(a)') YELLOW // trim(repo_name) // ":" // trim(branch_name) // " [ GIT MODE ]" // RESET // " | " // &
                                     YELLOW // "a:add u:unstage m:commit h:push l:pull f:fetch d:diff t:tag" // RESET // " | " // &
                                     DIM // "Space:select ↑↓:nav →:enter ←:back ~:home /:root 8:fav *:star " // &
                                     "alt-n:rename alt-v:view alt-m:move alt-y:copy alt-x:cut alt-p:paste alt-r:delete .:hidden " // &
                                     "alt-s:search alt-g:exit-mode alt-c:cd ctrl-q:quit" // RESET
        else if (in_git_repo) then
            ! Normal mode in git repo - show alt-g to enter git mode
            write(output_unit, '(a)') DIM // trim(repo_name) // ":" // trim(branch_name) // " | " // &
                                     "Space:select Shift+↑↓:block | ↑↓:nav →:enter ←:back ~:home /:root " // &
                                     "8:fav *:star alt-n:rename alt-v:view alt-m:move alt-y:copy alt-x:cut alt-p:paste alt-r:delete " // &
                                     ".:hidden alt-s:search alt-g:git-mode alt-c:cd ctrl-q:quit" // RESET
        else
            ! Non-git repo footer
            write(output_unit, '(a)') DIM // "Space:select Shift+↑↓:block | ↑↓:nav →:enter ←:back ~:home /:root " // &
                                     "8:fav *:star alt-n:rename alt-v:view alt-m:move alt-y:copy alt-x:cut alt-p:paste alt-r:delete " // &
                                     ".:hidden alt-s:search alt-c:cd ctrl-q:quit" // RESET
        end if

    contains
        function itoa(n) result(str)
            integer, intent(in) :: n
            character(len=10) :: str
            write(str, '(i0)') n
        end function itoa
    end subroutine draw_interface

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

end module ui_display
