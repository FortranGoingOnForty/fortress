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
                              in_git_repo, repo_name, branch_name)
        integer, intent(in) :: r, c, current_count, parent_count, selected, parent_selected
        integer, intent(in) :: scroll_offset, parent_scroll_offset
        character(len=*), intent(in) :: current_dir, repo_name, branch_name
        character(len=*), dimension(*), intent(in) :: current_files, parent_files
        logical, dimension(*), intent(in) :: current_is_dir, parent_is_dir
        logical, dimension(*), intent(in) :: current_is_exec, parent_is_exec
        logical, dimension(*), intent(in) :: current_is_staged, current_is_unstaged, current_is_untracked
        logical, dimension(*), intent(in) :: current_has_incoming
        logical, intent(in) :: in_git_repo
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
                                                  current_is_untracked(current_idx), &
                                                  current_has_incoming(current_idx), .true.)
                    end if
                    write(output_unit, '(a)') RESET
                else
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
        if (in_git_repo) then
            write(output_unit, '(a)') DIM // trim(repo_name) // ":" // trim(branch_name) // " | " // RESET // &
                                     DIM // "↑↓:nav →:enter ←:back s:search o:open d:diff a:add u:unstage m:commit f:fetch l:pull p:push t:tag c:cd q:quit" // RESET
        else
            write(output_unit, '(a)') DIM // "↑↓:nav →:enter ←:back s:search o:open c:cd q:quit" // RESET
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
