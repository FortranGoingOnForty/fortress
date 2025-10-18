program test_right_pane
    use terminal_screen, only: init_screen, cleanup_screen, clear_screen
    use ui_panes_buffered, only: draw_panes_buffered

    implicit none
    character(len=256) :: current_dir, parent_dir

    ! Set test directories
    current_dir = "."
    parent_dir = ".."

    ! Initialize screen
    call init_screen()
    call clear_screen()

    ! Draw the panes with buffered rendering
    write(*, '(a)') "Testing buffered rendering - both panes should show files:"
    write(*, *)

    call draw_panes_buffered(parent_dir, current_dir, 2, 1)

    ! Wait for user input
    write(*, *)
    write(*, '(a)') "Press Enter to exit test..."
    read(*, *)

    ! Cleanup
    call cleanup_screen()
    write(*, '(a)') "Test complete - if you saw files in both panes, buffered rendering works!"

end program test_right_pane