program fortress_v2
    use iso_fortran_env, only: output_unit
    implicit none

    ! File entry type
    type :: file_entry
        character(len=256) :: name = ""
        logical :: is_dir = .false.
    end type file_entry

    ! Constants
    integer, parameter :: MAX_FILES = 200
    character(len=*), parameter :: ESC = char(27)

    ! State variables
    type(file_entry), dimension(MAX_FILES) :: current_files, parent_files
    integer :: current_count, parent_count
    integer :: selected = 1
    character(len=512) :: current_path, parent_path
    logical :: running = .true.
    character(len=10) :: key
    integer :: rows, cols
    integer :: i

    ! Initialize
    call get_cwd(current_path)
    call get_parent(current_path, parent_path)

    ! Setup terminal
    call system("stty -icanon -echo min 1 time 0")
    call get_terminal_size(rows, cols)

    ! Main loop
    do while (running)
        ! Get directory contents
        call list_dir(current_path, current_files, current_count)
        call list_dir(parent_path, parent_files, parent_count)

        ! Draw screen
        call clear_screen()
        call draw_header(current_path)
        call draw_panes(parent_files, parent_count, current_files, current_count, selected, rows, cols)
        call draw_footer()

        ! Get keyboard input
        call get_key(key)

        ! Handle input
        select case(trim(key))
        case('A')  ! Up arrow
            if (selected > 1) selected = selected - 1
        case('B')  ! Down arrow
            if (selected < current_count) selected = selected + 1
        case('C')  ! Right arrow - enter directory
            if (selected <= current_count .and. current_files(selected)%is_dir) then
                if (trim(current_files(selected)%name) /= ".") then
                    call change_dir(current_path, current_files(selected)%name, parent_path)
                    selected = 1
                end if
            end if
        case('D')  ! Left arrow - go to parent
            if (trim(current_path) /= "/") then
                current_path = parent_path
                call get_parent(current_path, parent_path)
                selected = 1
            end if
        case('q', 'Q')
            running = .false.
        end select
    end do

    ! Cleanup
    call system("stty icanon echo")
    call clear_screen()
    write(output_unit, '(a)') "Thanks for using FORTRESS v2!"

contains

    subroutine get_cwd(path)
        character(len=*), intent(out) :: path
        integer :: unit, ios

        call execute_command_line("pwd > pwd_temp.txt", wait=.true.)
        open(newunit=unit, file="pwd_temp.txt", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)') path
            close(unit)
        else
            path = "."
        end if
        call execute_command_line("rm -f pwd_temp.txt", wait=.false.)
    end subroutine get_cwd

    subroutine get_parent(path, parent)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: parent
        integer :: last_slash

        last_slash = index(path, '/', back=.true.)
        if (last_slash > 1) then
            parent = path(1:last_slash-1)
        else
            parent = "/"
        end if
    end subroutine get_parent

    subroutine change_dir(current, new_name, parent)
        character(len=*), intent(inout) :: current
        character(len=*), intent(in) :: new_name
        character(len=*), intent(inout) :: parent

        if (trim(new_name) == "..") then
            if (trim(current) /= "/") then
                parent = current
                call get_parent(current, parent)
                current = parent
                call get_parent(current, parent)
            end if
        else
            parent = current
            if (trim(current) == "/") then
                current = "/" // trim(new_name)
            else
                current = trim(current) // "/" // trim(new_name)
            end if
        end if
    end subroutine change_dir

    subroutine list_dir(path, files, count)
        character(len=*), intent(in) :: path
        type(file_entry), dimension(MAX_FILES), intent(out) :: files
        integer, intent(out) :: count
        integer :: unit, ios
        character(len=512) :: cmd

        ! Initialize
        files%name = ""
        files%is_dir = .false.

        ! List files
        write(cmd, '(a)') "ls -1a " // trim(path) // " 2>/dev/null > ls_temp.txt"
        call execute_command_line(trim(cmd), wait=.true.)

        open(newunit=unit, file="ls_temp.txt", status='old', iostat=ios)
        if (ios /= 0) then
            count = 0
            return
        end if

        count = 0
        do
            count = count + 1
            if (count > MAX_FILES) exit
            read(unit, '(a)', iostat=ios) files(count)%name
            if (ios /= 0) then
                count = count - 1
                exit
            end if
            ! Check if directory
            call is_directory(path, files(count)%name, files(count)%is_dir)
        end do

        close(unit)
        call execute_command_line("rm -f ls_temp.txt", wait=.false.)
    end subroutine list_dir

    subroutine is_directory(base_path, name, is_dir)
        character(len=*), intent(in) :: base_path, name
        logical, intent(out) :: is_dir
        character(len=512) :: full_path
        integer :: stat

        if (trim(base_path) == "/") then
            full_path = "/" // trim(name)
        else
            full_path = trim(base_path) // "/" // trim(name)
        end if

        call execute_command_line("test -d '" // trim(full_path) // "'", &
                                 exitstat=stat, wait=.true.)
        is_dir = (stat == 0)
    end subroutine is_directory

    subroutine get_terminal_size(rows, cols)
        integer, intent(out) :: rows, cols
        integer :: unit, ios

        call execute_command_line("tput lines > size_temp.txt", wait=.true.)
        open(newunit=unit, file="size_temp.txt", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *) rows
            close(unit)
        else
            rows = 24
        end if

        call execute_command_line("tput cols > size_temp.txt", wait=.true.)
        open(newunit=unit, file="size_temp.txt", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, *) cols
            close(unit)
        else
            cols = 80
        end if

        call execute_command_line("rm -f size_temp.txt", wait=.false.)
    end subroutine get_terminal_size

    subroutine clear_screen()
        write(output_unit, '(a)', advance='no') ESC // "[2J" // ESC // "[H"
    end subroutine clear_screen

    subroutine draw_header(path)
        character(len=*), intent(in) :: path
        write(output_unit, '(a)') ESC // "[1m" // "FORTRESS v2" // ESC // "[0m - " // trim(path)
        write(output_unit, '(a)') repeat("=", 70)
    end subroutine draw_header

    subroutine draw_panes(p_files, p_count, c_files, c_count, sel, rows, cols)
        type(file_entry), dimension(*), intent(in) :: p_files, c_files
        integer, intent(in) :: p_count, c_count, sel, rows, cols
        integer :: i
        integer :: left_width, right_width
        character(len=30) :: left_text
        character(len=50) :: right_text

        left_width = cols * 3 / 10
        right_width = cols - left_width - 3

        do i = 1, min(rows - 5, max(p_count, c_count))
            ! Left pane
            if (i <= p_count) then
                left_text = p_files(i)%name
                if (p_files(i)%is_dir .and. trim(left_text) /= "." .and. trim(left_text) /= "..") then
                    left_text = trim(left_text) // "/"
                end if
                if (len_trim(left_text) > left_width) then
                    left_text = left_text(1:left_width-3) // "..."
                end if
                write(output_unit, '(a)', advance='no') ESC // "[2m" // left_text(1:left_width) // ESC // "[0m"
            else
                write(output_unit, '(a)', advance='no') repeat(" ", left_width)
            end if

            ! Separator
            write(output_unit, '(a)', advance='no') " | "

            ! Right pane
            if (i <= c_count) then
                right_text = c_files(i)%name
                if (c_files(i)%is_dir .and. trim(right_text) /= "." .and. trim(right_text) /= "..") then
                    right_text = trim(right_text) // "/"
                end if

                if (i == sel) then
                    write(output_unit, '(a)') ESC // "[7m" // trim(right_text) // ESC // "[0m"
                else
                    write(output_unit, '(a)') trim(right_text)
                end if
            else
                write(output_unit, *)
            end if
        end do
    end subroutine draw_panes

    subroutine draw_footer()
        write(output_unit, *)
        write(output_unit, '(a)') ESC // "[2m" // "↑↓: navigate | →: enter | ←: back | q: quit" // ESC // "[0m"
    end subroutine draw_footer

    subroutine get_key(key)
        character(len=*), intent(out) :: key
        character(len=1) :: ch

        key = ""
        read(*, '(a1)', advance='no', iostat=i) ch

        if (ch == ESC) then
            read(*, '(a1)', advance='no', iostat=i) ch
            if (ch == '[') then
                read(*, '(a1)', advance='no', iostat=i) ch
                key = ch
            end if
        else
            key = ch
        end if
    end subroutine get_key

end program fortress_v2