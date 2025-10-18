program simple_fortress
    use iso_fortran_env, only: output_unit, input_unit
    implicit none

    character(len=256) :: current_path
    character(len=256) :: parent_path
    character(len=256), dimension(100) :: files
    character(len=256), dimension(100) :: parent_files
    integer :: num_files, num_parent_files
    integer :: selected = 1
    integer :: i
    character(len=1) :: key
    logical :: running = .true.

    ! Start in current directory
    current_path = "."
    parent_path = ".."

    ! Main loop
    do while (running)
        ! Get file lists
        call get_files(current_path, files, num_files)
        call get_files(parent_path, parent_files, num_parent_files)

        ! Clear screen (simple way)
        write(output_unit, '(a)') char(27) // "[2J" // char(27) // "[H"

        ! Draw header
        write(output_unit, '(a)') "FORTRESS - Simple File Browser"
        write(output_unit, '(a)') "=============================="
        write(output_unit, *)

        ! Draw files in two columns
        write(output_unit, '(a)') "Parent Directory          | Current Directory"
        write(output_unit, '(a)') "------------------------- | -------------------------"

        do i = 1, max(num_parent_files, num_files)
            if (i <= num_parent_files) then
                write(output_unit, '(a25)', advance='no') adjustl(parent_files(i))
            else
                write(output_unit, '(a25)', advance='no') " "
            end if

            write(output_unit, '(a)', advance='no') " | "

            if (i <= num_files) then
                if (i == selected) then
                    write(output_unit, '(a)') "> " // trim(files(i))
                else
                    write(output_unit, '(a)') "  " // trim(files(i))
                end if
            else
                write(output_unit, *)
            end if
        end do

        ! Show controls
        write(output_unit, *)
        write(output_unit, '(a)') "Controls: j=down, k=up, q=quit"
        write(output_unit, '(a)', advance='no') "Command: "

        ! Get input
        read(input_unit, '(a1)') key

        select case(key)
        case('j', 'J')
            if (selected < num_files) selected = selected + 1
        case('k', 'K')
            if (selected > 1) selected = selected - 1
        case('q', 'Q')
            running = .false.
        end select
    end do

    write(output_unit, '(a)') "Goodbye!"

contains

    subroutine get_files(path, file_list, count)
        character(len=*), intent(in) :: path
        character(len=256), dimension(100), intent(out) :: file_list
        integer, intent(out) :: count

        character(len=512) :: cmd
        character(len=256) :: temp_file
        integer :: unit, ios

        ! Create temp file name
        temp_file = "temp_files.txt"

        ! List files to temp file
        write(cmd, '(a)') "ls -1 " // trim(path) // " > " // trim(temp_file) // " 2>/dev/null"
        call execute_command_line(cmd, wait=.true.)

        ! Read files
        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios /= 0) then
            count = 0
            return
        end if

        count = 0
        do
            count = count + 1
            if (count > 100) exit
            read(unit, '(a)', iostat=ios) file_list(count)
            if (ios /= 0) then
                count = count - 1
                exit
            end if
        end do

        close(unit)

        ! Clean up
        call execute_command_line("rm -f " // trim(temp_file), wait=.false.)
    end subroutine get_files

end program simple_fortress