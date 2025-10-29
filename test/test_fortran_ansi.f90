program test_ansi
    use iso_fortran_env, only: output_unit
    implicit none

    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: NOREVERSE = ESC // "[27m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: BOLD = ESC // "[1m"

    ! Test 1: Reverse line followed by normal lines
    write(output_unit, '(a)') "Test 1: One reversed line followed by normal lines"
    write(output_unit, '(a)', advance='no') REVERSE // BLUE // BOLD // "CURSOR LINE"
    write(output_unit, '(a)', advance='no') NOREVERSE
    write(output_unit, '(a)') RESET
    write(output_unit, '(a)') NOREVERSE // BLUE // BOLD // "Normal line 1"
    write(output_unit, '(a)') NOREVERSE // BLUE // BOLD // "Normal line 2"
    write(output_unit, '(a)') NOREVERSE // BLUE // BOLD // "Normal line 3"

    write(output_unit, *)
    write(output_unit, '(a)') "If lines 1-3 above are reversed, the bug is in Fortran's output handling"
    write(output_unit, '(a)') "If only CURSOR LINE is reversed, the fix is working"

end program test_ansi
