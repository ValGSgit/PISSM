program pissm
    use constants_mod
    use types_mod
    use globals_mod
    use interface_manager
    use utilities
    implicit none
    
    ! Initialize the interface system
    call initialize_interface()
    
    ! Show enhanced welcome message
    call print_header_enhanced()
    call set_cursor_position(8, 1)
    call print_colored('🎉 Welcome to PISSM - Personal Interface System Structure & Modifications 🎉', COLOR_BOLD//COLOR_BRIGHT_CYAN)
    call set_cursor_position(10, 1)
    call print_colored('✨ Enhanced file manager with powerful interactive interface ✨', COLOR_BRIGHT_MAGENTA)
    call set_cursor_position(12, 1)
    call print_colored('📁 Current Directory: ', COLOR_BRIGHT_YELLOW)
    call print_colored(trim(ui_state%current_path), COLOR_BRIGHT_CYAN//COLOR_BOLD)
    
    call set_cursor_position(14, 1)
    call print_colored('🚀 Features:', COLOR_BRIGHT_GREEN//COLOR_BOLD)
    call set_cursor_position(15, 3)
    call print_colored('• 📊 Detailed file information with permissions', COLOR_WHITE)
    call set_cursor_position(16, 3)
    call print_colored('• 🎨 Beautiful visual interface with color-coded elements', COLOR_WHITE)
    call set_cursor_position(17, 3)
    call print_colored('• ⚡ Fast navigation with vim-like keybindings', COLOR_WHITE)
    call set_cursor_position(18, 3)
    call print_colored('• 🔍 Powerful search capabilities', COLOR_WHITE)
    call set_cursor_position(19, 3)
    call print_colored('• 🗂️  Complete file management operations', COLOR_WHITE)
    
    call set_cursor_position(21, 1)
    call print_colored('💡 Press ', COLOR_BRIGHT_YELLOW)
    call print_colored('?', COLOR_BRIGHT_GREEN//COLOR_BOLD)
    call print_colored(' for help or ', COLOR_BRIGHT_YELLOW)
    call print_colored('q', COLOR_BRIGHT_RED//COLOR_BOLD)
    call print_colored(' to quit anytime', COLOR_BRIGHT_YELLOW)
    
    call set_cursor_position(SCREEN_HEIGHT - 4, 1)
    call print_colored('Press any key to start exploring...', COLOR_BOLD//COLOR_BRIGHT_WHITE)
    call wait_for_key()
    
    ! Main interface loop
    call main_interface_loop()
    
    ! Cleanup and exit
    call cleanup_interface()
    
end program pissm