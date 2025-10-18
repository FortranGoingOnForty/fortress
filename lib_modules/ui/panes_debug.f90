module ui_panes_debug
    use iso_fortran_env, only: output_unit, error_unit
    use terminal_screen, only: move_cursor, get_terminal_size
    use filesystem_ops, only: file_entry, list_directory, MAX_FILES
    implicit none
    private

    public :: draw_file_list_debug

    ! ANSI color codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: BOLD = ESC // "[1m"

contains

    subroutine draw_file_list_debug(start_row, start_col, width, height, dir_path, selected, is_dimmed)
        integer, intent(in) :: start_row, start_col, width, height, selected
        character(len=*), intent(in) :: dir_path
        logical, intent(in) :: is_dimmed
        type(file_entry), dimension(MAX_FILES) :: files
        integer :: i, row, col_pos, file_count
        character(len=256) :: fname

        ! Debug info
        write(error_unit, *) "=== draw_file_list_debug ==="
        write(error_unit, *) "start_row=", start_row, " start_col=", start_col
        write(error_unit, *) "width=", width, " height=", height
        write(error_unit, *) "dir_path='", trim(dir_path), "'"
        write(error_unit, *) "selected=", selected, " is_dimmed=", is_dimmed

        files = list_directory(dir_path)

        ! Count files
        file_count = 0
        do i = 1, MAX_FILES
            if (len_trim(files(i)%name) == 0) exit
            file_count = i
        end do
        write(error_unit, *) "Files found: ", file_count

        row = start_row
        do i = 1, min(MAX_FILES, height)
            if (len_trim(files(i)%name) == 0) exit
            if (row > start_row + height - 1) exit

            ! Move to position
            call move_cursor(row, start_col)

            ! Get filename
            fname = files(i)%name

            write(error_unit, '(a,i0,a,a,a,l1)') "File ", i, ": '", trim(fname), "' is_dir=", files(i)%is_dir

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

            write(error_unit, '(a,i0,a,i0,a,a)') "Writing at row ", row, " col ", start_col, ": ", trim(fname)

            ! Simple write for debugging
            if (i == selected .and. .not. is_dimmed) then
                write(output_unit, '(a)', advance='no') REVERSE // trim(fname) // RESET
            else if (files(i)%is_dir) then
                write(output_unit, '(a)', advance='no') BLUE // trim(fname) // RESET
            else
                write(output_unit, '(a)', advance='no') trim(fname)
            end if

            row = row + 1
        end do

        write(error_unit, *) "=== end draw_file_list_debug ==="
        flush(error_unit)
    end subroutine draw_file_list_debug

end module ui_panes_debug