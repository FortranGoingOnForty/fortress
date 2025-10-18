module filesystem_ops
    implicit none
    private

    public :: get_file_list, get_pwd, get_parent_path, join_path
    public :: find_in_parent, find_file_in_list, fzf_search, write_exit_dir
    public :: MAX_PATH, MAX_FILES

    integer, parameter :: MAX_PATH = 512
    integer, parameter :: MAX_FILES = 500

contains

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

    function find_file_in_list(target_path, files, count) result(idx)
        character(len=*), intent(in) :: target_path
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: idx, pos
        character(len=MAX_PATH) :: basename

        ! Extract basename from target_path
        pos = index(target_path, "/", back=.true.)
        if (pos > 0) then
            basename = target_path(pos+1:)
        else
            basename = target_path
        end if

        ! Search for the file in the list
        do idx = 1, count
            if (trim(files(idx)) == trim(basename)) return
        end do

        ! Default to first item if not found
        idx = 1
    end function find_file_in_list

    subroutine fzf_search(search_dir, result_path)
        character(len=*), intent(in) :: search_dir
        character(len=*), intent(out) :: result_path
        character(len=MAX_PATH) :: temp_file, fzf_cmd
        integer :: unit, ios, stat

        result_path = ""

        ! Create temp file for fzf output
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_fzf"

        ! Restore terminal for fzf
        call execute_command_line("stty icanon echo 2>/dev/null")

        ! Build fzf command: find files, pipe to fzf, save selection
        fzf_cmd = "cd '" // trim(search_dir) // "' && " // &
                  "find . -type f -o -type d | " // &
                  "sed 's|^\./||' | " // &
                  "fzf --height=40% --reverse --border --preview 'ls -lh {}' " // &
                  "> " // trim(temp_file) // " 2>/dev/null"

        ! Run fzf
        call execute_command_line(trim(fzf_cmd), exitstat=stat, wait=.true.)

        ! Restore raw mode
        call execute_command_line("stty -icanon -echo min 1 time 0 2>/dev/null")

        ! Read result if fzf succeeded
        if (stat == 0) then
            open(newunit=unit, file=temp_file, status='old', iostat=ios)
            if (ios == 0) then
                read(unit, '(a)', iostat=ios) result_path
                if (ios == 0) then
                    ! Convert relative path to absolute
                    result_path = join_path(search_dir, result_path)
                end if
                close(unit)
            end if
        end if

        ! Cleanup
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
    end subroutine fzf_search

    subroutine write_exit_dir(dir)
        character(len=*), intent(in) :: dir
        character(len=MAX_PATH) :: temp_file
        integer :: unit, ios

        ! Create temp file in HOME directory
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_cd"

        open(newunit=unit, file=temp_file, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, '(a)') trim(dir)
            close(unit)
        end if
    end subroutine write_exit_dir

end module filesystem_ops
