module filesystem_ops
    implicit none
    private

    public :: get_file_list, get_pwd, get_parent_path, join_path
    public :: find_in_parent, find_file_in_list, fzf_search, write_exit_dir
    public :: open_file_in_default_app
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

    subroutine open_file_in_default_app(filepath)
        character(len=*), intent(in) :: filepath
        character(len=MAX_PATH) :: editor, visual, platform, temp_file
        character(len=MAX_PATH*2) :: open_cmd
        integer :: stat, unit, ios

        ! Try $EDITOR first (preferred for text editing)
        call get_environment_variable("EDITOR", editor, status=stat)
        if (stat == 0 .and. len_trim(editor) > 0) then
            ! Restore terminal to normal mode (like fuss does for pager)
            call execute_command_line("stty sane < /dev/tty", exitstat=stat)

            ! Open with $EDITOR - wait for it to finish
            open_cmd = trim(editor) // " '" // trim(filepath) // "'"
            call execute_command_line(trim(open_cmd), exitstat=stat, wait=.true.)

            ! Restore raw mode
            call execute_command_line("stty -icanon -echo min 1 time 0 < /dev/tty", exitstat=stat)
            return
        end if

        ! Try $VISUAL as fallback
        call get_environment_variable("VISUAL", visual, status=stat)
        if (stat == 0 .and. len_trim(visual) > 0) then
            ! Restore terminal to normal mode
            call execute_command_line("stty sane < /dev/tty", exitstat=stat)

            ! Open with $VISUAL - wait for it to finish
            open_cmd = trim(visual) // " '" // trim(filepath) // "'"
            call execute_command_line(trim(open_cmd), exitstat=stat, wait=.true.)

            ! Restore raw mode
            call execute_command_line("stty -icanon -echo min 1 time 0 < /dev/tty", exitstat=stat)
            return
        end if

        ! Fall back to platform-specific default application opener
        ! Detect platform using uname
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fortress_platform"
        call execute_command_line("uname > " // trim(temp_file) // " 2>/dev/null", wait=.true.)

        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)', iostat=ios) platform
            close(unit)
        else
            platform = "unknown"
        end if
        call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")

        ! Restore terminal before launching (in case default app is terminal-based)
        call execute_command_line("stty sane < /dev/tty", exitstat=stat)

        ! Use platform-specific opener (run in background, don't wait)
        if (index(platform, "Darwin") > 0) then
            ! macOS - open launches apps in new windows (usually GUI)
            open_cmd = "open '" // trim(filepath) // "' 2>/dev/null &"
        else if (index(platform, "Linux") > 0) then
            ! Linux - xdg-open uses desktop environment defaults
            open_cmd = "xdg-open '" // trim(filepath) // "' 2>/dev/null &"
        else
            ! Unknown platform - try xdg-open as a reasonable default
            open_cmd = "xdg-open '" // trim(filepath) // "' 2>/dev/null &"
        end if

        call execute_command_line(trim(open_cmd), wait=.false.)

        ! Restore raw mode immediately (since we're not waiting for the app)
        call execute_command_line("stty -icanon -echo min 1 time 0 < /dev/tty", exitstat=stat)
    end subroutine open_file_in_default_app

end module filesystem_ops
