program test_display_sim
    use iso_fortran_env, only: output_unit
    implicit none

    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: NOREVERSE = ESC // "[27m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    integer :: i

    write(output_unit, '(a)') "Simulating fortress display rendering:"
    write(output_unit, *)

    ! Simulate rendering 5 lines in the middle pane
    do i = 1, 5
        ! Separator (like fortress does)
        write(output_unit, '(a)', advance='no') " │ "

        ! NOREVERSE at start of line (like our fix)
        write(output_unit, '(a)', advance='no') NOREVERSE

        ! Render line
        if (i == 3) then
            ! This is the cursor line - should be reversed
            write(output_unit, '(a)', advance='no') REVERSE // BLUE // BOLD // "CURSOR_LINE"
            write(output_unit, '(a)', advance='no') NOREVERSE
            write(output_unit, '(a)') RESET
        else
            ! Normal line - should NOT be reversed
            write(output_unit, '(a)', advance='no') BLUE // BOLD // "normal_line_" // char(48+i)
            write(output_unit, '(a)') RESET
        end if
    end do

    write(output_unit, *)
    write(output_unit, '(a)') "If ALL lines above are reversed, we have a deeper issue"
    write(output_unit, '(a)') "If only CURSOR_LINE is reversed, the code logic is correct"

end program test_display_sim
