module terminal_screen
    use iso_c_binding
    use iso_fortran_env, only: output_unit, input_unit
    implicit none
    private

    public :: init_screen, cleanup_screen, clear_screen, move_cursor, get_terminal_size

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CLEAR = ESC // "[2J"
    character(len=*), parameter :: HOME = ESC // "[H"
    character(len=*), parameter :: HIDE_CURSOR = ESC // "[?25l"
    character(len=*), parameter :: SHOW_CURSOR = ESC // "[?25h"

contains

    subroutine init_screen()
        ! Initialize terminal for raw mode
        call execute_command_line("stty -echo -icanon min 1 time 0", wait=.false.)
        write(output_unit, '(a)', advance='no') HIDE_CURSOR
        call clear_screen()
    end subroutine init_screen

    subroutine cleanup_screen()
        ! Restore terminal settings
        write(output_unit, '(a)', advance='no') SHOW_CURSOR
        call clear_screen()
        call execute_command_line("stty echo icanon", wait=.false.)
    end subroutine cleanup_screen

    subroutine clear_screen()
        write(output_unit, '(a)', advance='no') CLEAR // HOME
        flush(output_unit)
    end subroutine clear_screen

    subroutine move_cursor(row, col)
        integer, intent(in) :: row, col
        character(len=20) :: pos_str

        write(pos_str, '(a,"[",i0,";",i0,"H")') ESC, row, col
        write(output_unit, '(a)', advance='no') trim(pos_str)
    end subroutine move_cursor

    subroutine get_terminal_size(rows, cols)
        integer, intent(out) :: rows, cols
        character(len=20) :: env_lines, env_cols
        integer :: ios

        ! Default values
        rows = 24
        cols = 80

        ! Try environment variables first
        call get_environment_variable("LINES", env_lines)
        call get_environment_variable("COLUMNS", env_cols)

        if (len_trim(env_lines) > 0) then
            read(env_lines, *, iostat=ios) rows
            if (ios /= 0) rows = 24
        end if

        if (len_trim(env_cols) > 0) then
            read(env_cols, *, iostat=ios) cols
            if (ios /= 0) cols = 80
        end if

        ! If environment variables not set, try using ANSI escape sequence
        ! This is a more complex method but works on most terminals
        if (len_trim(env_lines) == 0 .or. len_trim(env_cols) == 0) then
            ! For now, use reasonable defaults that work on most terminals
            ! Could implement CSI DSR (Device Status Report) later
            rows = 40  ! Common terminal height
            cols = 100 ! Common terminal width
        end if
    end subroutine get_terminal_size

end module terminal_screen