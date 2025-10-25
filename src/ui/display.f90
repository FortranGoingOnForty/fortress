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
                              in_git_repo, repo_name, branch_name, &
                              move_mode, move_source_name, move_dest_selected, &
                              has_clipboard, clipboard_is_cut, clipboard_source_name)
        integer, intent(in) :: r, c, current_count, parent_count, selected, parent_selected
        integer, intent(in) :: scroll_offset, parent_scroll_offset
        character(len=*), intent(in) :: current_dir, repo_name, branch_name
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
        integer :: left_w, i, parent_idx, current_idx, vis_h
        character(len=256) :: fname
        character(len=20) :: color_code

        left_w = c * 3 / 10
        vis_h = r - 3  ! Visible height

        ! Header
        if (move_mode) then
            write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                     " | " // RED // "MOVE: " // trim(move_source_name) // RESET
        else if (has_clipboard) then
            if (clipboard_is_cut) then
                write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                         " | " // YELLOW // "CUT: " // trim(clipboard_source_name) // RESET
            else
                write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir) // &
                                         " | " // GREEN // "COPY: " // trim(clipboard_source_name) // RESET
            end if
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

                ! Check if this file is cut to clipboard (show in dark red)
                if (has_clipboard .and. clipboard_is_cut .and. &
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
                    ! Normal selection (not in move mode)
                    ! If file is cut, show selected with red background instead of default color
                    if (has_clipboard .and. clipboard_is_cut .and. &
                        trim(current_files(current_idx)) == trim(clipboard_source_name)) then
                        write(output_unit, '(a)', advance='no') REVERSE // RED // trim(fname)
                    else
                        write(output_unit, '(a)', advance='no') REVERSE // trim(color_code) // trim(fname)
                    end if
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
        if (move_mode) then
            write(output_unit, '(a)') RED // "MOVE MODE: " // RESET // &
                                     DIM // "↑↓:next/prev dir →:enter dir ←:parent v:move here q:cancel" // RESET
        else if (in_git_repo) then
            write(output_unit, '(a)') DIM // trim(repo_name) // ":" // trim(branch_name) // " | " // RESET // &
                                     DIM // "↑↓:nav →:enter ←:back s:search o:open n:rename r:remove v:move y:copy x:cut p:paste .:hidden a:add u:unstage m:commit d:diff f:fetch l:pull h:push c:cd q:quit" // RESET
        else
            write(output_unit, '(a)') DIM // "↑↓:nav →:enter ←:back s:search o:open n:rename r:remove v:move y:copy x:cut p:paste .:hidden c:cd q:quit" // RESET
        end if
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
