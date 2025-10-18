module preview_ops
    use filesystem_ops, only: MAX_PATH, join_path
    implicit none
    private

    public :: get_file_preview, check_bat_available
    public :: MAX_PREVIEW_LINES

    integer, parameter :: MAX_PREVIEW_LINES = 100
    logical, save :: bat_available = .false.
    logical, save :: bat_checked = .false.

contains

    function check_bat_available() result(has_bat)
        logical :: has_bat
        integer :: stat

        if (.not. bat_checked) then
            call execute_command_line("which bat > /dev/null 2>&1", exitstat=stat, wait=.true.)
            bat_available = (stat == 0)
            bat_checked = .true.
        end if
        has_bat = bat_available
    end function check_bat_available

    subroutine get_file_preview(filepath, is_dir, lines, line_count, max_lines)
        character(len=*), intent(in) :: filepath
        logical, intent(in) :: is_dir
        character(len=*), dimension(:), intent(out) :: lines
        integer, intent(out) :: line_count
        integer, intent(in) :: max_lines
        character(len=MAX_PATH) :: temp_file, cmd
        integer :: unit, ios, i
        logical :: has_bat

        line_count = 0

        ! Skip special files
        if (trim(filepath) == "." .or. trim(filepath) == "..") then
            lines(1) = "(special directory)"
            line_count = 1
            return
        end if

        ! Create temp file
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_preview"

        if (is_dir) then
            ! Preview directory with ls
            write(cmd, '(a,a,a,i0,a,a)') "ls -lh '", trim(filepath), "' 2>/dev/null | head -", &
                                         max_lines, " > ", trim(temp_file)
        else
            ! Check if file is likely binary
            call execute_command_line("file -b '" // trim(filepath) // "' | grep -q text", exitstat=ios, wait=.true.)

            if (ios == 0) then
                ! Text file - use bat or cat
                has_bat = check_bat_available()
                if (has_bat) then
                    ! Use bat with syntax highlighting, no line numbers, limited lines
                    write(cmd, '(a,i0,a,a,a,a)') "bat --style=plain --color=always --line-range :", &
                                                 max_lines, " '", trim(filepath), "' 2>/dev/null > ", trim(temp_file)
                else
                    ! Fallback to cat with head
                    write(cmd, '(a,a,a,i0,a,a)') "cat '", trim(filepath), "' 2>/dev/null | head -", &
                                                 max_lines, " > ", trim(temp_file)
                end if
            else
                ! Binary file - show file type info
                cmd = "file -b '" // trim(filepath) // "' > " // trim(temp_file)
            end if
        end if

        ! Execute preview command
        call execute_command_line(trim(cmd), wait=.true.)

        ! Read preview into lines array
        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios == 0) then
            do i = 1, max_lines
                read(unit, '(a)', iostat=ios) lines(i)
                if (ios /= 0) exit
                line_count = line_count + 1
            end do
            close(unit)
        end if

        ! Cleanup
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")

        ! Handle empty result
        if (line_count == 0) then
            lines(1) = "(empty or unreadable)"
            line_count = 1
        end if
    end subroutine get_file_preview

end module preview_ops
