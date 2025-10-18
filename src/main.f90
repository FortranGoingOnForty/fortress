program fortress_main
    use iso_fortran_env, only: output_unit, error_unit
    use terminal_screen, only: init_screen, cleanup_screen, clear_screen
    use terminal_input, only: get_key, KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_ENTER, KEY_QUIT
    use filesystem_ops, only: list_directory, get_parent_dir, get_current_dir, is_directory, &
                              file_entry, MAX_FILES
    use ui_panes, only: draw_panes, update_selection

    implicit none

    logical :: running
    integer :: key
    character(len=256) :: current_dir
    character(len=256) :: parent_dir
    character(len=256) :: new_dir
    integer :: selected_index
    type(file_entry), dimension(MAX_FILES) :: current_files
    integer :: file_count, i

    ! Initialize
    running = .true.
    selected_index = 1
    current_dir = get_current_dir()
    parent_dir = get_parent_dir(current_dir)

    ! Set up terminal
    call init_screen()

    ! Main loop
    do while (running)
        ! Get current directory contents
        current_files = list_directory(current_dir)

        ! Count actual files
        file_count = 0
        do i = 1, MAX_FILES
            if (len_trim(current_files(i)%name) == 0) exit
            file_count = i
        end do

        call clear_screen()
        call draw_panes(parent_dir, current_dir, selected_index)

        key = get_key()

        select case(key)
        case(KEY_UP)
            call update_selection(selected_index, -1)
        case(KEY_DOWN)
            if (selected_index < file_count) then
                call update_selection(selected_index, 1)
            end if
        case(KEY_LEFT)
            ! Go to parent directory
            if (trim(current_dir) /= "/") then
                current_dir = parent_dir
                parent_dir = get_parent_dir(current_dir)
                selected_index = 1
            end if
        case(KEY_RIGHT, KEY_ENTER)
            ! Enter directory or open file
            if (selected_index <= file_count) then
                if (current_files(selected_index)%is_dir) then
                    ! Navigate into directory
                    if (trim(current_files(selected_index)%name) == "..") then
                        ! Same as pressing left arrow
                        if (trim(current_dir) /= "/") then
                            current_dir = parent_dir
                            parent_dir = get_parent_dir(current_dir)
                            selected_index = 1
                        end if
                    else if (trim(current_files(selected_index)%name) /= ".") then
                        ! Enter subdirectory
                        new_dir = trim(current_dir) // "/" // trim(current_files(selected_index)%name)
                        if (is_directory(new_dir)) then
                            parent_dir = current_dir
                            current_dir = new_dir
                            selected_index = 1
                        end if
                    end if
                else
                    ! Open file - TODO: implement file opening with $EDITOR
                end if
            end if
        case(KEY_QUIT)
            running = .false.
        end select
    end do

    ! Cleanup
    call cleanup_screen()

    write(output_unit, *) "Thanks for using FORTRESS!"

end program fortress_main