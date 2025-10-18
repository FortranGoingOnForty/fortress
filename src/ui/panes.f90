module ui_panes
    use iso_fortran_env, only: output_unit
    use terminal_screen, only: move_cursor, get_terminal_size
    use filesystem_ops, only: file_entry, list_directory, MAX_FILES
    implicit none
    private

    public :: draw_panes, update_selection

    ! ANSI color codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: YELLOW = ESC // "[33m"

contains

    subroutine draw_panes(parent_dir, current_dir, selected_index)
        character(len=*), intent(in) :: parent_dir, current_dir
        integer, intent(in) :: selected_index
        integer :: rows, cols
        integer :: left_width, right_width
        type(file_entry), dimension(MAX_FILES) :: parent_files, current_files
        integer :: i

        call get_terminal_size(rows, cols)

        ! Calculate pane widths (30/70 split)
        left_width = cols * 3 / 10
        right_width = cols - left_width - 1  ! -1 for separator

        ! Draw header
        call move_cursor(1, 1)
        write(output_unit, '(a)', advance='no') BOLD // "FORTRESS" // RESET // &
            " - " // trim(current_dir)

        ! Draw separator line
        do i = 2, rows - 1
            call move_cursor(i, left_width + 1)
            write(output_unit, '(a)', advance='no') "│"
        end do

        ! Draw parent directory pane
        call draw_file_list(2, 1, left_width, rows - 2, parent_dir, -1)

        ! Draw current directory pane
        call draw_file_list(2, left_width + 2, right_width, rows - 2, &
                            current_dir, selected_index)

        ! Draw footer
        call move_cursor(rows, 1)
        write(output_unit, '(a)', advance='no') DIM // &
            "↑↓:navigate  →:enter  ←:back  Ctrl-Q:quit" // RESET

        flush(output_unit)
    end subroutine draw_panes

    subroutine draw_file_list(start_row, start_col, width, height, dir_path, selected)
        integer, intent(in) :: start_row, start_col, width, height, selected
        character(len=*), intent(in) :: dir_path
        type(file_entry), dimension(MAX_FILES) :: files
        integer :: i, row
        character(len=256) :: display_name

        files = list_directory(dir_path)

        row = start_row
        do i = 1, min(MAX_FILES, height)
            if (len_trim(files(i)%name) == 0) exit
            if (row > start_row + height - 1) exit

            call move_cursor(row, start_col)

            ! Format display name
            display_name = files(i)%name
            if (len_trim(display_name) > width - 2) then
                display_name = display_name(1:width-5) // "..."
            end if

            ! Apply highlighting and colors
            if (i == selected) then
                write(output_unit, '(a)', advance='no') REVERSE
            end if

            if (files(i)%is_dir) then
                write(output_unit, '(a)', advance='no') BLUE // display_name // "/" // RESET
            else
                write(output_unit, '(a)', advance='no') display_name
            end if

            if (i == selected) then
                write(output_unit, '(a)', advance='no') RESET
            end if

            row = row + 1
        end do
    end subroutine draw_file_list

    subroutine update_selection(selected, delta)
        integer, intent(inout) :: selected
        integer, intent(in) :: delta

        selected = max(1, selected + delta)
        ! TODO: Add upper bound check based on actual file count
    end subroutine update_selection

end module ui_panes