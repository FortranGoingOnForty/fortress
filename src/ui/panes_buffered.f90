module ui_panes_buffered
    use iso_fortran_env, only: output_unit, error_unit
    use terminal_screen, only: move_cursor, get_terminal_size
    use filesystem_ops, only: file_entry, list_directory, MAX_FILES
    implicit none
    private

    public :: draw_panes_buffered

    ! ANSI color codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: BLUE = ESC // "[34m"

contains

    subroutine draw_panes_buffered(parent_dir, current_dir, selected_index, parent_selected)
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
        write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir)

        ! Draw both panes
        call draw_both_panes(2, left_width, right_width, rows - 2, &
                            parent_dir, current_dir, parent_sel, selected_index)

        ! Draw footer
        call move_cursor(rows, 1)
        write(output_unit, '(a)') DIM // "↑↓:navigate  →:enter  ←:back  Ctrl-Q:quit" // RESET

        flush(output_unit)
    end subroutine draw_panes_buffered

    subroutine draw_both_panes(start_row, left_width, right_width, height, &
                              parent_dir, current_dir, parent_sel, current_sel)
        integer, intent(in) :: start_row, left_width, right_width, height
        character(len=*), intent(in) :: parent_dir, current_dir
        integer, intent(in) :: parent_sel, current_sel

        type(file_entry), dimension(MAX_FILES) :: parent_files, current_files
        integer :: row, i, j, file_count
        character(len=256) :: left_text, right_text

        ! Get files for both directories
        parent_files = list_directory(parent_dir)
        current_files = list_directory(current_dir)

        ! Count current files
        file_count = 0
        do i = 1, MAX_FILES
            if (len_trim(current_files(i)%name) == 0) exit
            file_count = i
        end do

        ! Draw each row
        do row = start_row, start_row + height - 1
            i = row - start_row + 1

            ! Build left pane text
            left_text = ""
            if (i <= MAX_FILES .and. len_trim(parent_files(i)%name) > 0) then
                left_text = format_file_entry(parent_files(i), i == parent_sel, .true., left_width)
            end if

            ! Build right pane text
            right_text = ""
            if (i <= MAX_FILES .and. len_trim(current_files(i)%name) > 0) then
                right_text = format_file_entry(current_files(i), i == current_sel, .false., right_width)
            end if

            ! Build complete line
            call move_cursor(row, 1)

            ! Write left pane
            write(output_unit, '(a)', advance='no') left_text

            ! Pad to separator
            do j = len_trim(left_text) + 1, left_width
                write(output_unit, '(a1)', advance='no') ' '
            end do

            ! Write separator
            write(output_unit, '(a)', advance='no') DIM // "│" // RESET

            ! Write right pane
            write(output_unit, '(a)', advance='no') right_text

            ! Clear rest of line
            do j = len_trim(right_text) + 1, right_width
                write(output_unit, '(a1)', advance='no') ' '
            end do
        end do
    end subroutine draw_both_panes

    function format_file_entry(entry, is_selected, is_dimmed, max_width) result(formatted)
        type(file_entry), intent(in) :: entry
        logical, intent(in) :: is_selected, is_dimmed
        integer, intent(in) :: max_width
        character(len=512) :: formatted
        character(len=256) :: name


        name = entry%name
        if (entry%is_dir .and. trim(name) /= "." .and. trim(name) /= "..") then
            name = trim(name) // "/"
        end if

        ! Truncate if needed
        if (len_trim(name) > max_width - 1) then
            name = name(1:max_width-4) // "..."
        end if

        ! Format with colors
        if (is_selected .and. .not. is_dimmed) then
            ! Active selection
            if (entry%is_dir) then
                formatted = REVERSE // BLUE // trim(name) // RESET
            else
                formatted = REVERSE // trim(name) // RESET
            end if
        else if (is_dimmed) then
            ! Dimmed pane
            if (is_selected) then
                if (entry%is_dir) then
                    formatted = DIM // BOLD // BLUE // trim(name) // RESET
                else
                    formatted = DIM // BOLD // trim(name) // RESET
                end if
            else
                if (entry%is_dir) then
                    formatted = DIM // BLUE // trim(name) // RESET
                else
                    formatted = DIM // trim(name) // RESET
                end if
            end if
        else
            ! Normal active pane
            if (entry%is_dir) then
                formatted = BLUE // trim(name) // RESET
            else
                formatted = trim(name)
            end if
        end if

    end function format_file_entry

end module ui_panes_buffered