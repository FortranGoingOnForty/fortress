program fortress_clean
    use iso_fortran_env, only: output_unit, error_unit
    implicit none

    ! Constants
    integer, parameter :: MAX_PATH = 512
    integer, parameter :: MAX_FILES = 500
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CLEAR = ESC // "[2J" // ESC // "[H"
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: REVERSE = ESC // "[7m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"

    ! Variables
    character(len=MAX_PATH) :: current_dir, parent_dir, temp_dir
    character(len=MAX_PATH), dimension(MAX_FILES) :: current_files, parent_files
    logical, dimension(MAX_FILES) :: current_is_dir, parent_is_dir
    logical, dimension(MAX_FILES) :: current_is_exec, parent_is_exec
    integer :: current_count, parent_count
    integer :: selected = 1
    integer :: parent_selected = -1
    character(len=1) :: key
    logical :: running = .true.
    integer :: i, rows, cols

    ! Initialize
    current_dir = get_pwd()
    parent_dir = get_parent_path(current_dir)

    ! Setup terminal
    call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")

    ! Main loop
    do while (running)
        ! Get files
        call get_file_list(current_dir, current_files, current_is_dir, current_is_exec, current_count)
        call get_file_list(parent_dir, parent_files, parent_is_dir, parent_is_exec, parent_count)

        ! Find current dir in parent
        parent_selected = find_in_parent(current_dir, parent_files, parent_count)

        ! Get terminal size
        call get_term_size(rows, cols)

        ! Draw interface
        write(output_unit, '(a)', advance='no') CLEAR
        call draw_interface(rows, cols)

        ! Get input
        read(*, '(a1)', advance='no') key

        ! Handle input
        select case(ichar(key))
        case(27)  ! ESC sequence
            call read_arrow_key(key)
            select case(key)
            case('A')  ! Up
                if (selected > 1) selected = selected - 1
            case('B')  ! Down
                if (selected < current_count) selected = selected + 1
            case('C')  ! Right - enter
                if (current_is_dir(selected)) then
                    if (trim(current_files(selected)) == "..") then
                        temp_dir = current_dir
                        current_dir = parent_dir
                        parent_dir = get_parent_path(current_dir)
                        selected = max(1, find_in_parent(temp_dir, current_files, MAX_FILES))
                    else if (trim(current_files(selected)) /= ".") then
                        parent_dir = current_dir
                        current_dir = join_path(current_dir, current_files(selected))
                        selected = 1
                    end if
                end if
            case('D')  ! Left - back
                if (current_dir /= "/") then
                    temp_dir = current_dir
                    current_dir = parent_dir
                    parent_dir = get_parent_path(current_dir)
                    selected = max(1, find_in_parent(temp_dir, current_files, MAX_FILES))
                end if
            end select
        case(113, 81)  ! 'q' or 'Q'
            running = .false.
        end select
    end do

    ! Cleanup
    call execute_command_line("stty icanon echo 2>/dev/null")
    write(output_unit, '(a)', advance='no') CLEAR
    write(output_unit, '(a)') "Thanks for using FORTRESS!"

contains

    function get_pwd() result(path)
        character(len=MAX_PATH) :: path
        integer :: unit, ios

        call execute_command_line("pwd > .fortress_pwd 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fortress_pwd", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)') path
            close(unit)
        else
            path = "."
        end if
        call execute_command_line("rm -f .fortress_pwd 2>/dev/null")
    end function get_pwd

    function get_parent_path(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=MAX_PATH) :: parent
        integer :: pos

        pos = index(path, "/", back=.true.)
        if (pos > 1) then
            parent = path(1:pos-1)
        else if (pos == 1) then
            parent = "/"
        else
            parent = "."
        end if
    end function get_parent_path

    function join_path(base, name) result(full)
        character(len=*), intent(in) :: base, name
        character(len=MAX_PATH) :: full

        if (base == "/") then
            full = "/" // trim(name)
        else
            full = trim(base) // "/" // trim(name)
        end if
    end function join_path

    function find_in_parent(dir, files, count) result(idx)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: idx, pos
        character(len=256) :: basename

        pos = index(dir, "/", back=.true.)
        if (pos > 0) then
            basename = dir(pos+1:)
        else
            basename = dir
        end if

        do idx = 1, count
            if (trim(files(idx)) == trim(basename)) return
        end do
        idx = 1
    end function find_in_parent

    subroutine get_file_list(dir, files, is_dir, is_exec, count)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(out) :: files
        logical, dimension(*), intent(out) :: is_dir, is_exec
        integer, intent(out) :: count
        integer :: unit, ios, stat
        character(len=MAX_PATH) :: fullpath

        call execute_command_line("ls -1a '" // trim(dir) // "' > .fortress_ls 2>/dev/null", wait=.true.)

        open(newunit=unit, file=".fortress_ls", status='old', iostat=ios)
        if (ios /= 0) then
            count = 0
            return
        end if

        count = 0
        do
            count = count + 1
            if (count > MAX_FILES) exit
            read(unit, '(a)', iostat=ios) files(count)
            if (ios /= 0) then
                count = count - 1
                exit
            end if

            fullpath = join_path(dir, files(count))
            call execute_command_line("test -d '" // trim(fullpath) // "'", exitstat=stat, wait=.true.)
            is_dir(count) = (stat == 0)

            ! Check if executable (but not directories)
            if (.not. is_dir(count)) then
                call execute_command_line("test -x '" // trim(fullpath) // "'", exitstat=stat, wait=.true.)
                is_exec(count) = (stat == 0)
            else
                is_exec(count) = .false.
            end if
        end do

        close(unit)
        call execute_command_line("rm -f .fortress_ls 2>/dev/null")
    end subroutine get_file_list

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

    subroutine draw_interface(r, c)
        integer, intent(in) :: r, c
        integer :: left_w, i
        character(len=256) :: fname
        character(len=20) :: color_code

        left_w = c * 3 / 10

        ! Header
        write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir)

        ! Files
        do i = 1, min(r-3, max(parent_count, current_count))
            ! Parent pane
            if (i <= parent_count) then
                fname = parent_files(i)
                if (parent_is_dir(i) .and. fname /= "." .and. fname /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for parent file
                color_code = get_file_color(parent_files(i), parent_is_dir(i), parent_is_exec(i))

                if (i == parent_selected) then
                    write(output_unit, '(a)', advance='no') DIM // BOLD // trim(color_code) // &
                        fname(1:min(len_trim(fname),left_w)) // RESET
                else
                    write(output_unit, '(a)', advance='no') DIM // trim(color_code) // &
                        fname(1:min(len_trim(fname),left_w)) // RESET
                end if
                write(output_unit, '(a)', advance='no') repeat(" ", max(0, left_w - len_trim(fname)))
            else
                write(output_unit, '(a)', advance='no') repeat(" ", left_w)
            end if

            ! Separator
            write(output_unit, '(a)', advance='no') " │ "

            ! Current pane
            if (i <= current_count) then
                fname = current_files(i)
                if (current_is_dir(i) .and. fname /= "." .and. fname /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for current file
                color_code = get_file_color(current_files(i), current_is_dir(i), current_is_exec(i))

                if (i == selected) then
                    write(output_unit, '(a)') REVERSE // trim(color_code) // trim(fname) // RESET
                else
                    write(output_unit, '(a)') trim(color_code) // trim(fname) // RESET
                end if
            else
                write(output_unit, *)
            end if
        end do

        ! Footer
        write(output_unit, '(a)') DIM // "↑↓:nav →:enter ←:back q:quit" // RESET
    end subroutine draw_interface

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

    function get_file_color(filename, is_dir, is_exec) result(color)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_dir, is_exec
        character(len=20) :: color

        ! Directories: Blue and bold
        if (is_dir) then
            color = BOLD // BLUE
        ! Dotfiles: Grey
        else if (filename(1:1) == '.') then
            color = GREY
        ! Executable files: Green
        else if (is_exec) then
            color = GREEN
        ! All other files: White
        else
            color = WHITE
        end if
    end function get_file_color

end program fortress_clean