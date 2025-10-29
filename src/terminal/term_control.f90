module terminal_control
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: get_term_size, setup_raw_mode, restore_terminal, read_arrow_key, read_arrow_key_with_shift
    public :: ESC, CLEAR, BOLD, DIM, REVERSE, NOREVERSE, RESET, UNDERLINE
    public :: BLUE, GREEN, RED, GREY, WHITE, YELLOW, BG_WHITE, BLACK
    public :: invalidate_term_cache

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CLEAR = ESC // "[2J" // ESC // "[H"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: UNDERLINE = ESC // "[4m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: NOREVERSE = ESC // "[27m"  ! Explicitly turn off reverse video
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: RED = ESC // "[31m"
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"
    character(len=*), parameter :: YELLOW = ESC // "[33m"
    character(len=*), parameter :: BLACK = ESC // "[30m"
    character(len=*), parameter :: BG_WHITE = ESC // "[47m"  ! White background

    ! Terminal size cache
    integer, save :: cached_rows = 0
    integer, save :: cached_cols = 0
    logical, save :: cache_valid = .false.
    integer, save :: cache_counter = 0
    integer, parameter :: CACHE_REFRESH_INTERVAL = 100  ! Refresh every 100 calls

contains

    subroutine invalidate_term_cache()
        cache_valid = .false.
    end subroutine invalidate_term_cache

    subroutine get_term_size(r, c)
        integer, intent(out) :: r, c
        integer :: unit, ios
        character(len=256) :: temp_file

        ! Increment counter for periodic refresh
        cache_counter = cache_counter + 1

        ! Use cache if valid and not time for refresh
        if (cache_valid .and. cache_counter < CACHE_REFRESH_INTERVAL) then
            r = cached_rows
            c = cached_cols
            return
        end if

        ! Reset counter when refreshing
        if (cache_counter >= CACHE_REFRESH_INTERVAL) cache_counter = 0

        ! Get fresh terminal size using single command
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_size"

        ! Get both dimensions in one command (more efficient)
        call execute_command_line("echo ""$(tput lines) $(tput cols)"" > " // trim(temp_file) // " 2>/dev/null", wait=.true.)

        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *, iostat=ios) r, c
            close(unit)
            if (ios /= 0) then
                ! Fallback to defaults if read fails
                r = 24
                c = 80
            end if
        else
            ! Fallback to defaults if file open fails
            r = 24
            c = 80
        end if

        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")

        ! Update cache
        cached_rows = r
        cached_cols = c
        cache_valid = .true.
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

    subroutine read_arrow_key_with_shift(k, is_shift)
        character(len=1), intent(out) :: k
        logical, intent(out) :: is_shift
        character(len=1) :: ch1, ch2, ch3, ch4

        is_shift = .false.
        k = ' '

        ! Read first character after ESC
        read(*, '(a1)', advance='no') ch1
        if (ch1 /= '[') then
            k = ch1
            return
        end if

        ! Read second character
        read(*, '(a1)', advance='no') ch2

        ! Check if it's a simple arrow (just a letter)
        if (ch2 == 'A' .or. ch2 == 'B' .or. ch2 == 'C' .or. ch2 == 'D') then
            k = ch2
            return
        end if

        ! Check for Shift+Arrow sequence: [1;2X where X is A/B/C/D
        if (ch2 == '1') then
            read(*, '(a1)', advance='no') ch3
            if (ch3 == ';') then
                read(*, '(a1)', advance='no') ch4
                if (ch4 == '2') then
                    ! This is a Shift+Arrow sequence
                    read(*, '(a1)', advance='no') k
                    is_shift = .true.
                    return
                end if
            end if
        end if

        ! If we get here, it's some other sequence, treat as regular arrow
        k = ch2
    end subroutine read_arrow_key_with_shift

end module terminal_control
