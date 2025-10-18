module terminal_control
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: get_term_size, setup_raw_mode, restore_terminal, read_arrow_key
    public :: ESC, CLEAR, BOLD, DIM, REVERSE, RESET
    public :: BLUE, GREEN, RED, GREY, WHITE

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CLEAR = ESC // "[2J" // ESC // "[H"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: RED = ESC // "[31m"
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"

contains

    subroutine get_term_size(r, c)
        integer, intent(out) :: r, c
        integer :: unit, ios

        call execute_command_line("tput lines > .fortress_size 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fortress_size", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *) r
            close(unit)
        else
            r = 24
        end if

        call execute_command_line("tput cols > .fortress_size 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fortress_size", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *) c
            close(unit)
        else
            c = 80
        end if

        call execute_command_line("rm -f .fortress_size 2>/dev/null")
    end subroutine get_term_size

    subroutine setup_raw_mode()
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")
    end subroutine setup_raw_mode

    subroutine restore_terminal()
        call execute_command_line("stty icanon echo 2>/dev/null")
    end subroutine restore_terminal

    subroutine read_arrow_key(k)
        character(len=1), intent(out) :: k
        character(len=1) :: ch

        read(*, '(a1)', advance='no') ch
        if (ch == '[') then
            read(*, '(a1)', advance='no') k
        else
            k = ch
        end if
    end subroutine read_arrow_key

end module terminal_control
