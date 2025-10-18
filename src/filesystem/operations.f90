module filesystem_ops
    use iso_c_binding
    use filesystem_c_interface
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
        type(c_ptr) :: dir_handle, entry_ptr
        type(c_dirent), pointer :: dir_entry
        type(c_stat_buf), target :: stat_buf
        character(kind=c_char, len=MAX_PATH), target :: c_path
        character(len=MAX_PATH) :: full_path
        integer :: count, i, stat_result

        ! Initialize entries
        do i = 1, MAX_FILES
            entries(i)%name = ""
            entries(i)%is_dir = .false.
            entries(i)%size = 0
        end do

        ! Convert Fortran string to C string
        c_path = trim(path) // c_null_char

        ! Open directory
        dir_handle = c_opendir(c_loc(c_path))
        if (.not. c_associated(dir_handle)) return

        count = 0
        do
            entry_ptr = c_readdir(dir_handle)
            if (.not. c_associated(entry_ptr)) exit

            count = count + 1
            if (count > MAX_FILES) exit

            call c_f_pointer(entry_ptr, dir_entry)

            ! Convert C string to Fortran string
            entries(count)%name = ""
            do i = 1, 255
                if (dir_entry%d_name(i) == c_null_char) exit
                entries(count)%name(i:i) = dir_entry%d_name(i)
            end do

            ! Get file stats
            full_path = trim(path) // "/" // trim(entries(count)%name)
            c_path = trim(full_path) // c_null_char
            stat_result = c_stat(c_loc(c_path), c_loc(stat_buf))

            if (stat_result == 0) then
                entries(count)%is_dir = S_ISDIR(stat_buf%st_mode)
                entries(count)%size = stat_buf%st_size
            end if
        end do

        ! Close directory
        i = c_closedir(dir_handle)
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
        type(c_stat_buf), target :: stat_buf
        character(kind=c_char, len=MAX_PATH), target :: c_path
        integer :: stat_result

        is_dir = .false.
        c_path = trim(path) // c_null_char
        stat_result = c_stat(c_loc(c_path), c_loc(stat_buf))

        if (stat_result == 0) then
            is_dir = S_ISDIR(stat_buf%st_mode)
        end if
    end function is_directory

    function get_current_dir() result(cwd)
        character(len=MAX_PATH) :: cwd
        character(kind=c_char, len=MAX_PATH), target :: c_buffer
        type(c_ptr) :: result_ptr

        c_buffer = ""
        result_ptr = c_getcwd(c_loc(c_buffer), int(MAX_PATH, c_size_t))

        if (c_associated(result_ptr)) then
            ! Convert C string to Fortran string
            cwd = ""
            block
                integer :: i
                do i = 1, MAX_PATH
                    if (c_buffer(i:i) == c_null_char) exit
                    cwd(i:i) = c_buffer(i:i)
                end do
            end block
        else
            cwd = "."
        end if
    end function get_current_dir

end module filesystem_ops