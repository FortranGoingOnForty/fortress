module filesystem_ops
    use iso_c_binding
    implicit none
    private

    public :: list_directory, get_parent_dir, is_directory, get_current_dir
    public :: file_entry, MAX_FILES

    integer, parameter :: MAX_FILES = 1000
    integer, parameter :: MAX_PATH = 256

    type :: file_entry
        character(len=MAX_PATH) :: name
        logical :: is_dir
        integer(c_long) :: size
    end type file_entry

contains

    function list_directory(path) result(entries)
        character(len=*), intent(in) :: path
        type(file_entry), dimension(MAX_FILES) :: entries
        character(len=MAX_PATH) :: temp_file
        character(len=MAX_PATH) :: line, full_path
        integer :: unit, ios, count, i, cmd_stat

        ! Initialize entries
        do i = 1, MAX_FILES
            entries(i)%name = ""
            entries(i)%is_dir = .false.
            entries(i)%size = 0
        end do

        ! Create a temporary file to store ls output in HOME directory
        ! This should work even in raw terminal mode
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_ls_temp.txt"

        ! Use ls -1 for simpler parsing with error checking
        ! Use explicit sh to ensure it works in raw terminal mode
        call execute_command_line("sh -c 'ls -1a """ // trim(path) // """ > " // trim(temp_file) // " 2>&1'", &
                                 exitstat=cmd_stat, wait=.true.)

        ! Open and read the temp file
        open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        count = 0

        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (count >= MAX_FILES) exit
            if (len_trim(line) == 0) cycle

            count = count + 1

            ! Store the filename
            entries(count)%name = trim(adjustl(line))

            ! Check if it's a directory using the is_directory function
            full_path = trim(path) // "/" // trim(entries(count)%name)
            entries(count)%is_dir = is_directory(full_path)

            ! For now, set size to 0 (could add stat later)
            entries(count)%size = 0
        end do

        close(unit)

        ! Clean up temp file
        call execute_command_line("rm -f " // trim(temp_file), wait=.false.)
    end function list_directory

    function get_parent_dir(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=MAX_PATH) :: parent
        integer :: last_slash

        parent = path

        ! Find last '/' in path
        last_slash = index(path, '/', back=.true.)
        if (last_slash > 1) then
            parent = path(1:last_slash-1)
        else if (last_slash == 1) then
            parent = "/"
        else
            parent = ".."
        end if
    end function get_parent_dir

    function is_directory(path) result(is_dir)
        character(len=*), intent(in) :: path
        logical :: is_dir
        integer :: stat

        is_dir = .false.

        ! Use test command to check if it's a directory
        call execute_command_line("sh -c 'test -d """ // trim(path) // """'", &
                                  exitstat=stat, wait=.true.)
        is_dir = (stat == 0)
    end function is_directory

    function get_current_dir() result(cwd)
        character(len=MAX_PATH) :: cwd
        character(len=MAX_PATH) :: temp_file
        integer :: unit, ios

        ! Create a temporary file to store pwd output
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_pwd_temp.txt"
        call execute_command_line("sh -c 'pwd > " // trim(temp_file) // "'", wait=.true.)

        ! Read the current directory
        open(newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            cwd = "."
        else
            read(unit, '(a)', iostat=ios) cwd
            if (ios /= 0) cwd = "."
            close(unit)
        end if

        ! Clean up temp file
        call execute_command_line("rm -f " // trim(temp_file), wait=.false.)
    end function get_current_dir

end module filesystem_ops