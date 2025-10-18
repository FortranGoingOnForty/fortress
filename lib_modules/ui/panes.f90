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
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"

contains

    subroutine draw_panes(parent_dir, current_dir, selected_index, parent_selected)
        character(len=*), intent(in) :: parent_dir, current_dir
        integer, intent(in) :: selected_index
        integer, intent(in), optional :: parent_selected
        integer :: rows, cols
        integer :: left_width, right_width
        integer :: i
        integer :: parent_sel

        ! Handle optional parent selection
        if (present(parent_selected)) then
            parent_sel = parent_selected
        else
            parent_sel = -1
        end if

        call get_terminal_size(rows, cols)

        ! Calculate pane widths (30/70 split)
        left_width = cols * 3 / 10
        right_width = cols - left_width - 1  ! -1 for separator

        ! Draw header
        call move_cursor(1, 1)
        ! Clear the header line first
        do i = 1, cols
            write(output_unit, '(a1)', advance='no') ' '
        end do
        call move_cursor(1, 1)
        write(output_unit, '(a)', advance='no') BOLD // "FORTRESS" // RESET // &
            " - " // trim(current_dir)

        ! Draw vertical separator
        do i = 2, rows - 1
            call move_cursor(i, left_width + 1)
            write(output_unit, '(a)', advance='no') DIM // "│" // RESET
        end do

        ! Draw parent directory pane (dimmed, with parent selection)
        call draw_file_list(2, 1, left_width, rows - 2, parent_dir, parent_sel, .true.)

        ! Draw current directory pane (active)
        call draw_file_list(2, left_width + 2, right_width, rows - 2, &
                            current_dir, selected_index, .false.)

        ! Draw footer
        call move_cursor(rows, 1)
        do i = 1, cols
            write(output_unit, '(a1)', advance='no') ' '
        end do
        call move_cursor(rows, 1)
        write(output_unit, '(a)', advance='no') DIM // &
            "↑↓:navigate  →:enter  ←:back  Ctrl-Q:quit" // RESET

        flush(output_unit)
    end subroutine draw_panes

    subroutine draw_file_list(start_row, start_col, width, height, dir_path, selected, is_dimmed)
        integer, intent(in) :: start_row, start_col, width, height, selected
        character(len=*), intent(in) :: dir_path
        logical, intent(in) :: is_dimmed
        type(file_entry), dimension(MAX_FILES) :: files
        integer :: i, row, col_pos
        character(len=256) :: fname

        files = list_directory(dir_path)

        row = start_row
        do i = 1, min(MAX_FILES, height)
            if (len_trim(files(i)%name) == 0) exit
            if (row > start_row + height - 1) exit

            ! Move to position
            call move_cursor(row, start_col)

            ! Get filename
            fname = files(i)%name

            ! Add slash for directories (not for . and ..)
            if (files(i)%is_dir) then
                if (trim(fname) /= "." .and. trim(fname) /= "..") then
                    fname = trim(fname) // "/"
                end if
            end if

            ! Truncate if too long
            if (len_trim(fname) > width - 2) then
                fname = fname(1:width-5) // "..."
            end if

            ! Draw the entry with proper colors
            if (i == selected .and. .not. is_dimmed) then
                ! Active selection - highlight whole line with reverse video
                write(output_unit, '(a)', advance='no') REVERSE // trim(get_file_color(files(i))) // &
                                                        trim(fname) // RESET
                ! Pad the rest of the line
                do col_pos = len_trim(fname) + 1, min(width - 1, 70)
                    write(output_unit, '(a)', advance='no') REVERSE // ' ' // RESET
                end do
            else
                ! Normal or dimmed entry
                if (is_dimmed) then
                    if (i == selected) then
                        ! Dimmed selection
                        write(output_unit, '(a)', advance='no') DIM // BOLD // &
                                                                trim(get_file_color(files(i))) // &
                                                                trim(fname) // RESET
                    else
                        ! Dimmed normal
                        write(output_unit, '(a)', advance='no') DIM // trim(get_file_color(files(i))) // &
                                                                trim(fname) // RESET
                    end if
                else
                    ! Active pane, normal entry
                    write(output_unit, '(a)', advance='no') trim(get_file_color(files(i))) // &
                                                            trim(fname) // RESET
                end if

                ! Clear rest of line
                do col_pos = len_trim(fname) + 1, min(width - 1, 70)
                    write(output_unit, '(a1)', advance='no') ' '
                end do
            end if

            row = row + 1
        end do

        ! Clear any remaining rows
        do while (row <= start_row + height - 1)
            call move_cursor(row, start_col)
            do col_pos = 1, min(width - 1, 70)
                write(output_unit, '(a1)', advance='no') ' '
            end do
            row = row + 1
        end do
    end subroutine draw_file_list

    subroutine update_selection(selected, delta)
        integer, intent(inout) :: selected
        integer, intent(in) :: delta

        selected = max(1, selected + delta)
        ! TODO: Add upper bound check based on actual file count
    end subroutine update_selection

    function get_file_color(entry) result(color)
        type(file_entry), intent(in) :: entry
        character(len=20) :: color

        ! Directories: Blue and bold
        if (entry%is_dir) then
            color = BOLD // BLUE
        ! Dotfiles: Grey
        else if (entry%name(1:1) == '.') then
            color = GREY
        ! Executable files: Green
        else if (entry%is_executable) then
            color = GREEN
        ! All other files: White (default)
        else
            color = WHITE
        end if
    end function get_file_color

end module ui_panes