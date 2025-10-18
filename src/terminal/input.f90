module terminal_input
    use iso_c_binding
    use iso_fortran_env, only: input_unit
    implicit none
    private

    public :: get_key
    public :: KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT
    public :: KEY_ENTER, KEY_SPACE, KEY_TAB, KEY_QUIT
    public :: KEY_UNKNOWN

    ! Key codes
    integer, parameter :: KEY_UNKNOWN = -1
    integer, parameter :: KEY_UP = 1
    integer, parameter :: KEY_DOWN = 2
    integer, parameter :: KEY_LEFT = 3
    integer, parameter :: KEY_RIGHT = 4
    integer, parameter :: KEY_ENTER = 10
    integer, parameter :: KEY_SPACE = 32
    integer, parameter :: KEY_TAB = 9
    integer, parameter :: KEY_QUIT = 17  ! Ctrl-Q

contains

    function get_key() result(key)
        integer :: key
        character :: ch
        integer :: ios

        key = KEY_UNKNOWN

        read(input_unit, '(a1)', advance='no', iostat=ios) ch
        if (ios /= 0) return

        ! Check for escape sequences (arrow keys)
        if (ichar(ch) == 27) then  ! ESC
            read(input_unit, '(a1)', advance='no', iostat=ios) ch
            if (ios /= 0) return
            if (ch == '[') then
                read(input_unit, '(a1)', advance='no', iostat=ios) ch
                if (ios /= 0) return
                select case(ch)
                case('A')
                    key = KEY_UP
                case('B')
                    key = KEY_DOWN
                case('C')
                    key = KEY_RIGHT
                case('D')
                    key = KEY_LEFT
                end select
            end if
        else
            ! Regular characters
            select case(ichar(ch))
            case(10, 13)  ! Enter/Return
                key = KEY_ENTER
            case(32)  ! Space
                key = KEY_SPACE
            case(9)   ! Tab
                key = KEY_TAB
            case(17)  ! Ctrl-Q
                key = KEY_QUIT
            case(104) ! 'h' for left
                key = KEY_LEFT
            case(106) ! 'j' for down
                key = KEY_DOWN
            case(107) ! 'k' for up
                key = KEY_UP
            case(108) ! 'l' for right
                key = KEY_RIGHT
            case(113) ! 'q' for quit
                key = KEY_QUIT
            end select
        end if
    end function get_key

end module terminal_input