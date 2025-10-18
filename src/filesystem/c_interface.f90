module filesystem_c_interface
    use iso_c_binding
    implicit none
    private

    public :: c_getcwd, c_opendir, c_readdir, c_closedir
    public :: c_stat, c_dirent, c_stat_buf
    public :: S_ISDIR

    ! Constants for file types
    integer(c_int), parameter :: S_IFMT  = int(o'170000', c_int)
    integer(c_int), parameter :: S_IFDIR = int(o'040000', c_int)
    integer(c_int), parameter :: S_IFREG = int(o'100000', c_int)

    ! C struct dirent (simplified)
    type, bind(c) :: c_dirent
        integer(c_long) :: d_ino        ! inode number
        integer(c_long) :: d_off        ! offset
        integer(c_short) :: d_reclen    ! record length
        integer(c_char) :: d_type       ! file type
        character(c_char) :: d_name(256) ! filename
    end type c_dirent

    ! C struct stat (simplified)
    type, bind(c) :: c_stat_buf
        integer(c_long) :: st_dev
        integer(c_long) :: st_ino
        integer(c_int) :: st_mode
        integer(c_long) :: st_nlink
        integer(c_int) :: st_uid
        integer(c_int) :: st_gid
        integer(c_long) :: st_rdev
        integer(c_long) :: st_size
        integer(c_long) :: st_blksize
        integer(c_long) :: st_blocks
        integer(c_long) :: st_atime
        integer(c_long) :: st_mtime
        integer(c_long) :: st_ctime
    end type c_stat_buf

    interface
        function c_getcwd(buf, size) bind(c, name="getcwd")
            import :: c_ptr, c_size_t
            type(c_ptr) :: c_getcwd
            type(c_ptr), value :: buf
            integer(c_size_t), value :: size
        end function c_getcwd

        function c_opendir(dirname) bind(c, name="opendir")
            import :: c_ptr
            type(c_ptr) :: c_opendir
            type(c_ptr), value :: dirname
        end function c_opendir

        function c_readdir(dirp) bind(c, name="readdir")
            import :: c_ptr
            type(c_ptr) :: c_readdir
            type(c_ptr), value :: dirp
        end function c_readdir

        function c_closedir(dirp) bind(c, name="closedir")
            import :: c_int, c_ptr
            integer(c_int) :: c_closedir
            type(c_ptr), value :: dirp
        end function c_closedir

        function c_stat(path, buf) bind(c, name="stat")
            import :: c_int, c_ptr
            integer(c_int) :: c_stat
            type(c_ptr), value :: path
            type(c_ptr), value :: buf
        end function c_stat
    end interface

contains

    logical function S_ISDIR(mode)
        integer(c_int), intent(in) :: mode
        S_ISDIR = (iand(mode, S_IFMT) == S_IFDIR)
    end function S_ISDIR

end module filesystem_c_interface