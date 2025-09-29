program pissm
    use constants_mod
    use types_mod
    use globals_mod
    use interface_manager
    use utilities
    implicit none
    integer :: term_width, term_height
    
    ! Set up comprehensive cleanup for all exit scenarios
    call execute_command_line('trap "stty sane echo; tput cnorm; exit 0" EXIT')
    call execute_command_line('trap "stty sane echo; tput cnorm; exit 130" INT')  ! Ctrl+C
    call execute_command_line('trap "stty sane echo; tput cnorm; exit 0" TERM')   ! Terminate
    
    ! Initialize dynamic terminal sizing (like neovim)
    call get_terminal_size(term_width, term_height)
    ! Update global screen dimensions
    SCREEN_WIDTH = term_width
    SCREEN_HEIGHT = term_height
    
    ! Initialize the interface system
    call initialize_interface()
    
    ! Start directly with the file manager interface (like neovim)
    ! Main interface loop
    call main_interface_loop()
    
    ! Cleanup and exit normally
    call cleanup_interface()
    
    ! Final terminal restoration
    call execute_command_line('stty sane echo 2>/dev/null')
    call execute_command_line('tput cnorm 2>/dev/null')
    
end program pissm