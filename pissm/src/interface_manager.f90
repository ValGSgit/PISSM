module interface_manager
    use constants_mod
    use types_mod
    use globals_mod
    use utilities
    use directory_tree
    use file_operations
    use user_input
    implicit none
    
contains

    subroutine initialize_interface()
        character(len=MAX_PATH_LENGTH) :: pwd
        integer :: i, access_result
        
        ! Initialize variables
        pwd = ''
        i = 0
        access_result = 0
        
        ! Initialize terminal for raw input
        call setup_terminal()
        
        ! Initialize UI state first to prevent undefined values
        ui_state%current_selection = 1
        ui_state%scroll_position = 0
        ui_state%view_mode = 1
        ui_state%show_hidden = .false.
        ui_state%clipboard_path = ''
        ui_state%clipboard_operation = 0
        ui_state%previous_path = ''
        ui_state%previous_selection = 1
        
        ! Get current working directory with fallbacks
        call get_environment_variable('PWD', pwd)
        if (len_trim(pwd) == 0) then
            pwd = '.'
            ! Try to get absolute path
            call execute_command_line('pwd > /tmp/pissm_pwd 2>/dev/null')
            open(unit=99, file='/tmp/pissm_pwd', status='old', iostat=i)
            if (i == 0) then
                read(99, '(A)', iostat=i) pwd
                if (i /= 0) pwd = '.'
                close(99)
                call execute_command_line('rm -f /tmp/pissm_pwd')
            end if
        end if
        
        ! Validate directory access before proceeding
        call test_directory_access(pwd, access_result)
        if (access_result /= 0) then
            pwd = '.'  ! Fall back to current directory
            call test_directory_access(pwd, access_result)
            if (access_result /= 0) then
                pwd = '/'  ! Final fallback to root
            end if
        end if
        
        ui_state%current_path = trim(pwd)
        original_path = ui_state%current_path
        
        ! Load initial directory with error handling
        call create_directory_node(ui_state%current_path, current_node)
        if (associated(current_node)) then
            ! Don't store separate root_node - just use current_node
            call refresh_directory_node(current_node, ui_state%show_hidden)
        else
            ! Critical error - cannot initialize
            call restore_terminal()
            write(*,*) 'Error: Cannot access any directory. Check permissions.'
            stop 1
        end if
    end subroutine initialize_interface

    subroutine main_interface_loop()
        character(len=1) :: key
        logical :: need_refresh = .true.
        integer :: key_code, term_width, term_height
        
        call clear_screen()
        call display_interface()
        
        do while (program_running)            
            if (need_refresh) then
                call display_interface()
                need_refresh = .false.
            end if
            
            key = get_key()
            key_code = iachar(key)
            
            ! Handle Ctrl+C (ASCII 3) and Ctrl+D (ASCII 4) for clean exit
            if (key_code == 3 .or. key_code == 4) then
                program_running = .false.
                cycle
            end if
            
            ! Ignore null characters and other control characters that might cause issues
            if (key_code == 0 .or. (key_code > 0 .and. key_code < 32 .and. key_code /= 10 .and. key_code /= 13)) then
                if (key_code /= 3 .and. key_code /= 4) then  ! Don't ignore exit signals
                    cycle  ! Skip processing
                end if
            end if
            
            select case (key)
                case ('j', 'J')  ! Move down
                    call handle_move_down()
                    call adjust_scroll_position()
                    need_refresh = .true.
                    
                case ('k', 'K')  ! Move up
                    call handle_move_up()
                    call adjust_scroll_position()
                    need_refresh = .true.
                    
                case (char(10), char(13))  ! Enter - open/enter
                    call handle_enter_key()
                    need_refresh = .true.
                    
                case ('>')  ! Right arrow - only enter directories
                    call handle_right_arrow()
                    need_refresh = .true.
                    
                case ('b', '<')  ! Back/Left arrow - go up directory  
                    call handle_back_operation()
                    need_refresh = .true.
                    
                case ('c', 'C')  ! Copy
                    call handle_copy_operation()
                    ! Don't refresh display for copy operation
                    
                case ('m', 'M')  ! Move
                    call handle_move_operation()
                    ! Don't refresh display for move operation
                    
                case ('d', 'D')  ! Delete
                    call handle_delete_operation()
                    need_refresh = .true.
                    
                case ('n', 'N')  ! New file/directory
                    call handle_new_operation()
                    need_refresh = .true.
                    
                case ('s', 'S')  ! Search
                    call handle_search_operation()
                    need_refresh = .true.
                    
                case ('p', 'P')  ! Paste
                    call handle_paste_operation()
                    need_refresh = .true.
                    
                case ('r', 'R')  ! Refresh and update terminal size
                    ! Update terminal dimensions in case window was resized
                    call get_terminal_size(term_width, term_height)
                    ! Update global screen dimensions
                    SCREEN_WIDTH = term_width
                    SCREEN_HEIGHT = term_height
                    call refresh_directory_node(current_node, ui_state%show_hidden)
                    need_refresh = .true.
                    
                case ('h', 'H')  ! Toggle hidden files
                    ui_state%show_hidden = .not. ui_state%show_hidden
                    call refresh_directory_node(current_node, ui_state%show_hidden)
                    need_refresh = .true.
                    
                case ('g')  ! Goto operations
                    call handle_goto_operation()
                    need_refresh = .true.
                    
                case ('f', 'F')  ! File details
                    call show_file_details()
                    need_refresh = .true.
                    
                case ('/')  ! Quick search
                    call handle_quick_search()
                    need_refresh = .true.
                    
                case ('~')  ! Go to home directory
                    call handle_home_directory()
                    need_refresh = .true.
                    
                case ('u', 'U')  ! Page Up (fast scroll up)
                    call handle_page_up()
                    call adjust_scroll_position()
                    need_refresh = .true.
                    
                case ('i', 'I')  ! Page Down (fast scroll down)  
                    call handle_page_down()
                    call adjust_scroll_position()
                    need_refresh = .true.
                    
                case ('w', 'W')  ! Jump to top
                    call handle_jump_to_top()
                    call adjust_scroll_position()
                    need_refresh = .true.
                    
                case ('e', 'E')  ! Jump to bottom (End)
                    call handle_jump_to_bottom()
                    call adjust_scroll_position()
                    need_refresh = .true.
                    
                case ('?')  ! Help
                    call show_help()
                    need_refresh = .true.
                    
                case ('q', 'Q')  ! Quit
                    program_running = .false.
                    
                case default
                    ! Ignore unrecognized keys silently
                    continue
            end select
        end do
        
        ! Cleanup
        call cleanup_interface()
    end subroutine main_interface_loop

    subroutine display_interface()
        integer :: start_line, visible_lines, max_visible_lines
        character(len=200) :: status_line
        
        call clear_screen()
        call print_header()
        
        ! Draw breadcrumb
        call draw_breadcrumb(ui_state%current_path)
        
        start_line = 4
        max_visible_lines = SCREEN_HEIGHT - 8
        visible_lines = min(current_node%file_count, max_visible_lines)
        
        call display_directory_tree(current_node, ui_state%scroll_position, &
                                   max_visible_lines, ui_state%current_selection)
        
        ! Status bar with file info and scroll indicators
        if (current_node%file_count > max_visible_lines) then
            write(status_line, '(A,I0,A,I0,A,I0,A,I0,A)') '📊 Files: ', current_node%file_count, &
                                           ' │ Showing: ', ui_state%scroll_position + 1, '-', &
                                           min(ui_state%scroll_position + max_visible_lines, current_node%file_count), &
                                           ' │ Selected: ', ui_state%current_selection, ' │ '
        else
            write(status_line, '(A,I0,A,I0,A)') '📊 Files: ', current_node%file_count, &
                                               ' │ Selected: ', ui_state%current_selection, ' │ '
        end if
        
        if (ui_state%show_hidden) then
            status_line = trim(status_line)//'👁️  Hidden: ON │ '
        else
            status_line = trim(status_line)//'🙈 Hidden: OFF │ '
        end if
        
        if (len_trim(ui_state%clipboard_path) > 0) then
            if (ui_state%clipboard_operation == 1) then
                status_line = trim(status_line)//'📋 Clipboard: COPY'
            else
                status_line = trim(status_line)//'✂️  Clipboard: MOVE'
            end if
        end if
        
        call print_enhanced_footer(status_line)
        
        ! Enhanced command help with icons
        call set_cursor_position(SCREEN_HEIGHT - 1, 1)
        call print_colored('↑↓:Navigate | Enter:Open | →:Dir Only | ←:Back | c:Copy | m:Move | d:Delete | q:Quit', &
                          COLOR_BRIGHT_WHITE)
        
    end subroutine display_interface
    
    subroutine handle_move_down()
        if (current_node%file_count > 0) then
            if (ui_state%current_selection < current_node%file_count) then
                ui_state%current_selection = ui_state%current_selection + 1
            else
                ! Wrap to first item when at the last item
                ui_state%current_selection = 1
            end if
        end if
        ! Ensure selection is within bounds
        if (ui_state%current_selection > current_node%file_count .and. current_node%file_count > 0) then
            ui_state%current_selection = current_node%file_count
        end if
    end subroutine handle_move_down
    
    subroutine handle_move_up()
        if (current_node%file_count > 0) then
            if (ui_state%current_selection > 1) then
                ui_state%current_selection = ui_state%current_selection - 1
            else
                ! Wrap to last item when at the first item
                ui_state%current_selection = current_node%file_count
            end if
        end if
        ! Ensure selection is within bounds
        if (ui_state%current_selection < 1) then
            ui_state%current_selection = 1
        end if
    end subroutine handle_move_up

    subroutine handle_enter_key()
        character(len=MAX_PATH_LENGTH) :: selected_path
        type(directory_node), pointer :: new_node
        integer :: access_result
        
        ! Initialize variables
        selected_path = ''
        nullify(new_node)
        access_result = 0
        
        ! Safety checks
        if (.not. associated(current_node)) return
        if (current_node%file_count == 0) return
        if (ui_state%current_selection < 1 .or. ui_state%current_selection > current_node%file_count) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        
        if (is_selected_directory(current_node, ui_state%current_selection)) then
            ! Check if we can access the directory first
            call test_directory_access(selected_path, access_result)
            if (access_result /= 0) then
                call show_message('Error: Permission denied or directory not accessible')
                return
            end if
            
            ! Try to enter directory
            call create_directory_node(selected_path, new_node)
            if (associated(new_node)) then
                ! Store current position for navigation memory
                ui_state%previous_path = ui_state%current_path
                ui_state%previous_selection = ui_state%current_selection
                
                ! Clean up old node before switching
                if (associated(current_node)) then
                    call cleanup_directory_tree(current_node)
                end if
                ! Successfully created directory node - safe to navigate
                current_node => new_node
                ui_state%current_path = selected_path
                ui_state%current_selection = 1
                ui_state%scroll_position = 0
                
                call refresh_directory_node(current_node, ui_state%show_hidden)
            else
                call show_message('Error: Cannot access directory')
            end if
        else
            ! Check if it's a text file that we can edit
            if (is_text_file(selected_path)) then
                ! Ask user if they want to edit or open externally
                call show_file_action_menu(selected_path)
            else
                ! Try to open file with default program
                call open_file(selected_path)
                ! Don't call show_message here as it interferes with key handling
            end if
        end if
    end subroutine handle_enter_key

    subroutine handle_right_arrow()
        character(len=MAX_PATH_LENGTH) :: selected_path
        type(directory_node), pointer :: new_node
        integer :: access_result
        
        ! Initialize variables
        selected_path = ''
        nullify(new_node)
        access_result = 0
        
        if (current_node%file_count == 0) return
        
        ! Right arrow only works on directories
        if (.not. is_selected_directory(current_node, ui_state%current_selection)) then
            call show_message('Right arrow only works on directories. Use Enter to open files.')
            return
        end if
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        
        ! Check if we can access the directory first
        call test_directory_access(selected_path, access_result)
        if (access_result /= 0) then
            call show_message('Error: Permission denied or directory not accessible')
            return
        end if
        
        ! Try to enter directory
        call create_directory_node(selected_path, new_node)
        if (associated(new_node)) then
            ! Store current path for navigation memory before switching
            ui_state%previous_path = ui_state%current_path
            ui_state%previous_selection = ui_state%current_selection
            
            ! Clean up old node before switching (always safe to clean up)
            if (associated(current_node)) then
                call cleanup_directory_tree(current_node)
            end if
            ! Successfully created directory node - safe to navigate
            current_node => new_node
            ui_state%current_path = selected_path
            ui_state%current_selection = 1
            ui_state%scroll_position = 0
            
            call refresh_directory_node(current_node, ui_state%show_hidden)
        else
            call show_message('Error: Cannot access directory')
        end if
    end subroutine handle_right_arrow

    subroutine handle_copy_operation()
        character(len=MAX_PATH_LENGTH) :: selected_path
        
        if (current_node%file_count == 0) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        ui_state%clipboard_path = selected_path
        ui_state%clipboard_operation = 1
        
        call show_message('File copied to clipboard: '//trim(selected_path))
    end subroutine handle_copy_operation

    subroutine handle_move_operation()
        character(len=MAX_PATH_LENGTH) :: selected_path
        
        if (current_node%file_count == 0) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        ui_state%clipboard_path = selected_path
        ui_state%clipboard_operation = 2
        
        call show_message('File cut to clipboard: '//trim(selected_path))
    end subroutine handle_move_operation

    subroutine handle_delete_operation()
        character(len=MAX_PATH_LENGTH) :: selected_path
        character(len=200) :: confirmation_msg
        logical :: success
        
        if (current_node%file_count == 0) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        
        write(confirmation_msg, '(A)') 'Delete "'//trim(selected_path)//'"? [y/N]: '
        
        if (get_confirmation(confirmation_msg)) then
            success = delete_file(selected_path)
            if (success) then
                call show_message('File deleted successfully')
                call refresh_directory_node(current_node, ui_state%show_hidden)
                if (ui_state%current_selection > current_node%file_count) then
                    ui_state%current_selection = current_node%file_count
                end if
            else
                call show_message('Error: Failed to delete file')
            end if
        end if
    end subroutine handle_delete_operation

    subroutine handle_new_operation()
        character(len=1) :: choice
        character(len=MAX_FILENAME) :: new_name
        character(len=MAX_PATH_LENGTH) :: full_path
        logical :: success
        
        write(*, '(A)', advance='no') 'Create (f)ile or (d)irectory? '
        choice = get_key()
        
        if (choice /= 'f' .and. choice /= 'F' .and. choice /= 'd' .and. choice /= 'D') then
            call show_message('Invalid selection')
            return
        end if
        
        call get_string('Enter name: ', new_name, MAX_FILENAME)
        if (len_trim(new_name) == 0) then
            call show_message('Invalid name')
            return
        end if
        
        full_path = trim(ui_state%current_path)//'/'//trim(new_name)
        
        if (choice == 'f' .or. choice == 'F') then
            success = create_file(full_path)
        else
            success = create_directory(full_path)
        end if
        
        if (success) then
            call show_message('Created successfully')
            call refresh_directory_node(current_node, ui_state%show_hidden)
        else
            call show_message('Error: Failed to create')
        end if
    end subroutine handle_new_operation

    subroutine handle_search_operation()
        character(len=100) :: search_pattern
        type(file_entry), allocatable :: search_results(:)
        integer :: result_count, i, selection
        character(len=1) :: key
        
        call get_string('Search pattern: ', search_pattern, 100)
        
        if (len_trim(search_pattern) == 0) return
        
        ! Allocate search results array
        allocate(search_results(MAX_FILES))
        
        call search_files(ui_state%current_path, search_pattern, search_results, result_count)
        
        if (result_count == 0) then
            call show_message('No files found matching pattern')
            return
        end if
        
        ! Display search results with enhanced interface
        selection = 1
        do
            call print_header_enhanced()
            
            call set_cursor_position(4, 1)
            call print_colored('🔍 Search Results', COLOR_BOLD//COLOR_BRIGHT_CYAN)
            
            call set_cursor_position(5, 1)
            call print_colored(repeat('═', SCREEN_WIDTH), COLOR_CYAN)
            
            do i = 1, min(result_count, SCREEN_HEIGHT - 12)
                call set_cursor_position(8 + i, 1)
                if (i == selection) then
                    call print_colored(' ► ', COLOR_BRIGHT_YELLOW)
                else
                    call print_colored('   ', COLOR_WHITE)
                end if
                
                if (search_results(i)%is_directory) then
                    call print_colored('📁 ', COLOR_YELLOW)
                else
                    call print_colored('📄 ', COLOR_WHITE)
                end if
                
                call print_colored(trim(search_results(i)%full_path), COLOR_BRIGHT_WHITE)
            end do
            
            call print_enhanced_footer('j/k:Move │ Enter:Open │ q:Back')
            
            key = get_key()
            select case (key)
                case ('q', 'Q')
                    exit
                case ('j', 'J')
                    if (selection < result_count) selection = selection + 1
                case ('k', 'K')
                    if (selection > 1) selection = selection - 1
                case (char(10), char(13))
                    call open_file(search_results(selection)%full_path)
                    exit
            end select
        end do
        
        ! Clean up allocated memory
        if (allocated(search_results)) deallocate(search_results)
        
    end subroutine handle_search_operation

    subroutine handle_paste_operation()
        character(len=MAX_PATH_LENGTH) :: dest_path
        character(len=MAX_FILENAME) :: filename
        logical :: success
        
        success = .false.
        
        if (len_trim(ui_state%clipboard_path) == 0) then
            call show_message('Clipboard is empty')
            return
        end if
        
        call extract_filename(ui_state%clipboard_path, filename)
        dest_path = trim(ui_state%current_path)//'/'//trim(filename)
        
        if (ui_state%clipboard_operation == 1) then
            success = copy_file(ui_state%clipboard_path, dest_path)
            if (success) then
                call show_message('File copied successfully')
            else
                call show_message('Error: Failed to copy file')
            end if
        else if (ui_state%clipboard_operation == 2) then
            success = move_file(ui_state%clipboard_path, dest_path)
            if (success) then
                call show_message('File moved successfully')
                ui_state%clipboard_path = ''
                ui_state%clipboard_operation = 0
            else
                call show_message('Error: Failed to move file')
            end if
        end if
        
        if (success) then
            call refresh_directory_node(current_node, ui_state%show_hidden)
        end if
    end subroutine handle_paste_operation

    subroutine handle_back_operation()
        type(directory_node), pointer :: parent_node
        character(len=MAX_PATH_LENGTH) :: parent_path
        integer :: pos, access_result
        
        ! Initialize variables
        nullify(parent_node)
        parent_path = ''
        pos = 0
        access_result = 0
        
        ! Check if we can go back
        if (trim(ui_state%current_path) == '/' .or. len_trim(ui_state%current_path) <= 1) then
            call show_message('Already at root directory')
            return
        end if
        
        ! Calculate parent path
        pos = index(ui_state%current_path, '/', .true.)
        if (pos > 1) then
            parent_path = ui_state%current_path(1:pos-1)
        else
            parent_path = '/'
        end if
        
        ! Validate parent path before trying to navigate
        if (len_trim(parent_path) == 0) then
            call show_message('Error: Invalid parent directory path')
            return
        end if
        
        ! Test if we can access the parent directory
        call test_directory_access(parent_path, access_result)
        if (access_result /= 0) then
            call show_message('Error: Cannot access parent directory')
            return
        end if
        
        ! Create new directory node for parent
        call create_directory_node(parent_path, parent_node)
        if (associated(parent_node)) then
            ! Clean up old node before switching
            if (associated(current_node)) then
                call cleanup_directory_tree(current_node)
            end if
            ! Store the path of directory we're exiting (for cursor positioning)
            call find_directory_in_parent(parent_node, ui_state%current_path)
            
            ! Update navigation state - safe to proceed
            current_node => parent_node
            ui_state%current_path = parent_path
            
            ui_state%scroll_position = 0
            
            call refresh_directory_node(current_node, ui_state%show_hidden)
            call adjust_scroll_position()  ! Make sure cursor is visible
        else
            call show_message('Error: Failed to create parent directory node')
        end if
    end subroutine handle_back_operation

    subroutine open_file(filepath)
        character(len=*), intent(in) :: filepath
        character(len=1024) :: command
        
        ! Save terminal state before opening external program
        call execute_command_line('stty -g > /tmp/pissm_stty_backup 2>/dev/null')
        
        command = 'xdg-open "'//trim(filepath)//'" 2>/dev/null &'
        call execute_command_line(command)
        
        ! Brief pause to let the external program start
        call execute_command_line('sleep 0.1')
        
        ! Restore terminal state
        call execute_command_line('stty $(cat /tmp/pissm_stty_backup 2>/dev/null) 2>/dev/null')
        call execute_command_line('rm -f /tmp/pissm_stty_backup 2>/dev/null')
        
    end subroutine open_file

    subroutine show_message(message)
        character(len=*), intent(in) :: message
        character :: dummy_key
        integer :: i, iostat
        
        call set_cursor_position(SCREEN_HEIGHT - 3, 1)
        call print_colored(trim(message), COLOR_YELLOW//COLOR_BOLD)
        call print_colored(' (press any key)', COLOR_DIM)
        
        ! Wait for any key but ensure we consume it completely
        dummy_key = get_key()
        
        ! If we got an escape sequence, make sure we consume the whole thing
        if (iachar(dummy_key) == 27) then
            do i = 1, 5  ! Consume up to 5 more characters to clear escape sequence
                call execute_command_line('stty raw -echo min 0 time 1 2>/dev/null')
                read(*, '(A)', advance='no', iostat=iostat) dummy_key
                call execute_command_line('stty sane 2>/dev/null')
                if (iostat /= 0) exit  ! No more characters available
            end do
        end if
        
        ! Clear the message line completely
        call set_cursor_position(SCREEN_HEIGHT - 3, 1)
        call print_colored(repeat(' ', SCREEN_WIDTH), COLOR_RESET)
    end subroutine show_message

    subroutine show_help()
        call clear_screen()
        call print_header_enhanced()
        
        call set_cursor_position(6, 1)
        call print_colored('🆘 PISSM Help - Keyboard Commands', COLOR_BOLD//COLOR_BRIGHT_YELLOW)
        
        call set_cursor_position(7, 1)
        call print_colored(repeat('═', 50), COLOR_YELLOW)
        
        call set_cursor_position(9, 3)
        call print_colored('📍 Navigation:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(10, 3)
        call print_colored('j/k or ↓/↑   ', COLOR_GREEN)
        call print_colored('- Move selection up/down', COLOR_WHITE)
        call set_cursor_position(11, 3)
        call print_colored('Enter        ', COLOR_GREEN)
        call print_colored('- Open file/directory', COLOR_WHITE)
        call set_cursor_position(12, 3)
        call print_colored('→ (Right)    ', COLOR_GREEN)
        call print_colored('- Enter directory only', COLOR_WHITE)
        call set_cursor_position(13, 3)
        call print_colored('b or ←       ', COLOR_GREEN)
        call print_colored('- Go back one directory', COLOR_WHITE)
        call set_cursor_position(14, 3)
        call print_colored('gg           ', COLOR_GREEN)
        call print_colored('- Go to beginning of list', COLOR_WHITE)
        call set_cursor_position(15, 3)
        call print_colored('G            ', COLOR_GREEN)
        call print_colored('- Go to end of list', COLOR_WHITE)
        call set_cursor_position(16, 3)
        call print_colored('u/i          ', COLOR_GREEN)
        call print_colored('- Page Up/Page Down (fast scroll)', COLOR_WHITE)
        call set_cursor_position(17, 3)
        call print_colored('w/e          ', COLOR_GREEN)
        call print_colored('- Jump to top/bottom instantly', COLOR_WHITE)
        
        call set_cursor_position(18, 3)
        call print_colored('🔍 Search & View:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(19, 3)
        call print_colored('/            ', COLOR_GREEN)
        call print_colored('- Quick search in current directory', COLOR_WHITE)
        call set_cursor_position(20, 3)
        call print_colored('f            ', COLOR_GREEN)
        call print_colored('- Show detailed file information', COLOR_WHITE)
        call set_cursor_position(20, 3)
        call print_colored('h            ', COLOR_GREEN)
        call print_colored('- Toggle hidden files visibility', COLOR_WHITE)
        call set_cursor_position(21, 3)
        call print_colored('r            ', COLOR_GREEN)
        call print_colored('- Refresh directory listing', COLOR_WHITE)
        
        call set_cursor_position(23, 3)
        call print_colored('🗂️  File Operations:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(24, 3)
        call print_colored('c            ', COLOR_GREEN)
        call print_colored('- Copy selected file to clipboard', COLOR_WHITE)
        call set_cursor_position(25, 3)
        call print_colored('m            ', COLOR_GREEN)
        call print_colored('- Move selected file to clipboard', COLOR_WHITE)
        call set_cursor_position(26, 3)
        call print_colored('p            ', COLOR_GREEN)
        call print_colored('- Paste from clipboard', COLOR_WHITE)
        call set_cursor_position(27, 3)
        call print_colored('d            ', COLOR_GREEN)
        call print_colored('- Delete selected file/directory', COLOR_WHITE)
        call set_cursor_position(28, 3)
        call print_colored('n            ', COLOR_GREEN)
        call print_colored('- Create new file or directory', COLOR_WHITE)
        
        call set_cursor_position(30, 3)
        call print_colored('⚙️  System:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(31, 3)
        call print_colored('?            ', COLOR_GREEN)
        call print_colored('- Show this help screen', COLOR_WHITE)
        call set_cursor_position(32, 3)
        call print_colored('q            ', COLOR_GREEN)
        call print_colored('- Quit PISSM', COLOR_WHITE)
        
        call set_cursor_position(34, 1)
        call print_colored('💡 Tips:', COLOR_BRIGHT_MAGENTA//COLOR_BOLD)
        call set_cursor_position(35, 3)
        call print_colored('• Use emoji indicators to identify file types quickly', COLOR_BRIGHT_WHITE)
        call set_cursor_position(36, 3)
        call print_colored('• Arrow keys work like vim keybindings (hjkl)', COLOR_BRIGHT_WHITE)
        call set_cursor_position(37, 3)
        call print_colored('• File sizes are automatically formatted (B, KB, MB, GB, TB)', COLOR_BRIGHT_WHITE)
        
        call wait_for_key()
    end subroutine show_help

    subroutine cleanup_interface()
        ! Ensure terminal is always restored first, regardless of other operations
        call restore_terminal()
        call execute_command_line('stty sane echo 2>/dev/null')
        call execute_command_line('tput cnorm 2>/dev/null')  ! Ensure cursor is visible
        
        ! Clear screen and show goodbye message
        call clear_screen()
        call set_cursor_position(1, 1)
        call print_colored('Thanks for using PISSM!', COLOR_GREEN//COLOR_BOLD)
        write(*,*) ''  ! Add a newline
        
        ! Clean up current directory tree
        if (associated(current_node)) then
            call cleanup_directory_tree(current_node)
            nullify(current_node)
        end if
    end subroutine cleanup_interface
    
    subroutine adjust_scroll_position()
        integer :: max_visible_lines, margin
        
        max_visible_lines = SCREEN_HEIGHT - 8
        margin = 3  ! Keep cursor 3 lines from edge for smoother scrolling
        
        ! If the list fits on screen, don't scroll
        if (current_node%file_count <= max_visible_lines) then
            ui_state%scroll_position = 0
            return
        end if
        
        ! Smooth scrolling with margins - start scrolling before hitting edges
        if (ui_state%current_selection <= ui_state%scroll_position + margin) then
            ! Scrolling up - keep cursor near top with margin
            ui_state%scroll_position = max(0, ui_state%current_selection - margin - 1)
        else if (ui_state%current_selection >= ui_state%scroll_position + max_visible_lines - margin) then
            ! Scrolling down - keep cursor near bottom with margin
            ui_state%scroll_position = min(current_node%file_count - max_visible_lines, &
                                         ui_state%current_selection - max_visible_lines + margin)
        end if
        
        ! Ensure scroll position stays within bounds
        ui_state%scroll_position = max(0, min(ui_state%scroll_position, &
                                             current_node%file_count - max_visible_lines))
    end subroutine adjust_scroll_position
    
    subroutine handle_goto_operation()
        character :: second_key
        
        ! Wait for second key
        second_key = get_key()
        
        select case (second_key)
            case ('g', 'G')  ! Go to beginning
                ui_state%current_selection = 1
                ui_state%scroll_position = 0
            case default  ! Go to end
                if (current_node%file_count > 0) then
                    ui_state%current_selection = current_node%file_count
                    call adjust_scroll_position()
                end if
        end select
    end subroutine handle_goto_operation
    
    subroutine show_file_details()
        character(len=MAX_PATH_LENGTH) :: selected_path
        character(len=1024) :: command, temp_file, line
        character(len=50) :: file_type
        integer :: unit_num, iostat
        
        if (current_node%file_count == 0) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        
        call clear_screen()
        call print_header_enhanced()
        
        call set_cursor_position(6, 1)
        call print_colored('📋 File Details', COLOR_BOLD//COLOR_BRIGHT_CYAN)
        
        call set_cursor_position(8, 3)
        call print_colored('Name: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(current_node%files(ui_state%current_selection)%name), COLOR_WHITE)
        
        call set_cursor_position(10, 3)
        call print_colored('Full Path: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(selected_path), COLOR_BRIGHT_CYAN)
        
        call set_cursor_position(12, 3)
        call print_colored('Permissions: ', COLOR_BRIGHT_YELLOW)
        call print_permissions_colored(current_node%files(ui_state%current_selection)%permissions)
        
        call set_cursor_position(14, 3)
        call print_colored('Owner/Group: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(current_node%files(ui_state%current_selection)%owner_group), COLOR_GREEN)
        
        if (.not. current_node%files(ui_state%current_selection)%is_directory) then
            call set_cursor_position(16, 3)
            call print_colored('Size: ', COLOR_BRIGHT_YELLOW)
            call format_size(current_node%files(ui_state%current_selection)%size, line)
            call print_colored(trim(line), COLOR_BRIGHT_MAGENTA)
        end if
        
        call set_cursor_position(18, 3)
        call print_colored('Modified: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(current_node%files(ui_state%current_selection)%modified_time), COLOR_BRIGHT_MAGENTA)
        
        ! Try to get file type
        if (.not. current_node%files(ui_state%current_selection)%is_directory) then
            temp_file = '/tmp/pissm_file_details'
            command = 'file "'//trim(selected_path)//'" > '//trim(temp_file)//' 2>/dev/null'
            call execute_command_line(command)
            
            open(newunit=unit_num, file=temp_file, status='old', iostat=iostat)
            if (iostat == 0) then
                read(unit_num, '(A)', iostat=iostat) line
                close(unit_num)
                call execute_command_line('rm -f '//trim(temp_file))
                
                if (iostat == 0) then
                    iostat = index(line, ': ')
                    if (iostat > 0) then
                        file_type = trim(adjustl(line(iostat+2:)))
                        call set_cursor_position(20, 3)
                        call print_colored('Type: ', COLOR_BRIGHT_YELLOW)
                        call print_colored(trim(file_type), COLOR_BRIGHT_WHITE)
                    end if
                end if
            end if
        end if
        
        call set_cursor_position(SCREEN_HEIGHT - 2, 1)
        call print_colored('Press any key to return...', COLOR_BRIGHT_GREEN)
        call wait_for_key()
    end subroutine show_file_details
    
    subroutine handle_quick_search()
        character(len=MAX_FILENAME) :: search_pattern
        character(len=100) :: line
        integer :: i, match_count, first_match
        character(len=MAX_FILENAME) :: lowercase_name, lowercase_pattern
        
        call get_string('Quick search: ', search_pattern, MAX_FILENAME)
        
        if (len_trim(search_pattern) == 0) return
        
        lowercase_pattern = search_pattern
        call to_lowercase(lowercase_pattern)
        
        match_count = 0
        first_match = 0
        
        do i = 1, current_node%file_count
            lowercase_name = current_node%files(i)%name
            call to_lowercase(lowercase_name)
            
            if (index(lowercase_name, trim(lowercase_pattern)) > 0) then
                match_count = match_count + 1
                if (first_match == 0) first_match = i
            end if
        end do
        
        if (first_match > 0) then
            ui_state%current_selection = first_match
            call adjust_scroll_position()
            write(line, '(A,I0,A)') 'Found ', match_count, ' matches'
            call show_message(trim(line))
        else
            call show_message('No matches found')
        end if
    end subroutine handle_quick_search
    
    subroutine handle_home_directory()
        character(len=MAX_PATH_LENGTH) :: home_path
        type(directory_node), pointer :: new_node
        
        call get_environment_variable('HOME', home_path)
        if (len_trim(home_path) == 0) home_path = '/'
        
        call create_directory_node(home_path, new_node)
        if (associated(new_node)) then
            ! Clean up old node before switching
            if (associated(current_node)) then
                call cleanup_directory_tree(current_node)
            end if
            current_node => new_node
            ui_state%current_path = home_path
            ui_state%current_selection = 1
            ui_state%scroll_position = 0
            call refresh_directory_node(current_node, ui_state%show_hidden)
        else
            call show_message('Error: Cannot access home directory')
        end if
    end subroutine handle_home_directory

    subroutine test_directory_access(path, result_code)
        character(len=*), intent(in) :: path
        integer, intent(out) :: result_code
        character(len=1024) :: command
        
        ! Initialize command string fully
        command = ''
        ! Use test command to check if directory is accessible
        write(command, '(A,A,A,A,A)') 'test -d "', trim(path), '" -a -r "', trim(path), '"'
        call execute_command_line(command, exitstat=result_code)
    end subroutine test_directory_access

    ! Check if a file is a text file that can be edited
    function is_text_file(filepath) result(is_text)
        character(len=*), intent(in) :: filepath
        logical :: is_text
        character(len=20) :: extension
        integer :: dot_pos
        
        is_text = .false.
        
        ! Get file extension
        dot_pos = index(filepath, '.', .true.)
        if (dot_pos > 0) then
            extension = filepath(dot_pos+1:)
            call to_lowercase(extension)
            
            select case (trim(extension))
                case ('txt', 'md', 'f90', 'f95', 'f03', 'f08', 'c', 'cpp', 'cxx', 'h', 'hpp', &
                      'py', 'java', 'js', 'html', 'htm', 'css', 'xml', 'json', 'yaml', 'yml', &
                      'sh', 'bash', 'zsh', 'conf', 'cfg', 'ini', 'log', 'csv', 'sql', 'r', &
                      'rb', 'php', 'pl', 'go', 'rs', 'kt', 'swift', 'scala', 'lua', 'vim')
                    is_text = .true.
                case default
                    is_text = .false.
            end select
        end if
    end function is_text_file

    ! Show menu for file action (edit vs open externally)
    subroutine show_file_action_menu(filepath)
        character(len=*), intent(in) :: filepath
        character :: choice
        character(len=256) :: filename
        integer :: slash_pos
        
        ! Extract filename from path
        slash_pos = index(filepath, '/', .true.)
        if (slash_pos > 0) then
            filename = filepath(slash_pos+1:)
        else
            filename = filepath
        end if
        
        call clear_screen()
        call print_colored('File Action Menu', COLOR_BOLD//COLOR_CYAN)
        write(*,*)
        write(*,*)
        call print_colored('File: '//trim(filename), COLOR_WHITE//COLOR_BOLD)
        write(*,*)
        write(*,*)
        write(*,'(A)') '  [E] Edit with nano (lightweight editor)'
        write(*,'(A)') '  [V] Edit with vim (advanced editor)'
        write(*,'(A)') '  [O] Open with external program'
        write(*,'(A)') '  [B] Back to file manager'
        write(*,*)
        write(*,'(A)', advance='no') 'Choice: '
        
        choice = get_key()
        
        select case (choice)
            case ('e', 'E')
                call launch_nano_editor(filepath)
            case ('v', 'V')
                call launch_vim_editor(filepath)
            case ('o', 'O')
                call open_file(filepath)
            case ('b', 'B', char(27))  ! B or ESC
                return
            case default
                return
        end select
        
    end subroutine show_file_action_menu

    ! Launch nano editor for a file
    subroutine launch_nano_editor(filepath)
        character(len=*), intent(in) :: filepath
        character(len=1024) :: command
        integer :: exit_code
        
        ! Properly restore terminal to cooked mode
        write(*,'(A)', advance='no') char(27)//'[?25h'  ! Show cursor
        call execute_command_line('stty sane echo 2>/dev/null')
        
        ! Build and execute nano command with proper error handling
        write(command, '(A,A,A)') 'nano "', trim(filepath), '"'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Reinitialize terminal for raw input
        call setup_terminal()
        
        ! Refresh display with proper terminal size
        call get_terminal_size(SCREEN_WIDTH, SCREEN_HEIGHT)
        call clear_screen()
        call display_interface()
        
    end subroutine launch_nano_editor

    ! Launch vim editor for a file  
    subroutine launch_vim_editor(filepath)
        character(len=*), intent(in) :: filepath
        character(len=1024) :: command
        integer :: exit_code
        
        ! Properly restore terminal to cooked mode
        write(*,'(A)', advance='no') char(27)//'[?25h'  ! Show cursor
        call execute_command_line('stty sane echo 2>/dev/null')
        
        ! Build and execute vim command with proper error handling
        write(command, '(A,A,A)') 'vim "', trim(filepath), '"'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Reinitialize terminal for raw input
        call setup_terminal()
        
        ! Refresh display with proper terminal size
        call get_terminal_size(SCREEN_WIDTH, SCREEN_HEIGHT)
        call clear_screen()
        call display_interface()
        
    end subroutine launch_vim_editor

    ! Find the directory we came from in the parent and position cursor on it
    subroutine find_directory_in_parent(parent_node, child_path)
        type(directory_node), pointer, intent(in) :: parent_node
        character(len=*), intent(in) :: child_path
        character(len=MAX_FILENAME) :: child_name
        integer :: i, slash_pos
        
        ! Extract the directory name we just exited
        child_name = ''
        slash_pos = 0
        
        ! Find the last slash to get the directory name
        do i = len_trim(child_path), 1, -1
            if (child_path(i:i) == '/') then
                slash_pos = i
                exit
            end if
        end do
        
        ! Extract directory name
        if (slash_pos > 0 .and. slash_pos < len_trim(child_path)) then
            child_name = child_path(slash_pos+1:len_trim(child_path))
        else
            ! Fallback - use the whole path as name
            child_name = trim(child_path)
        end if
        
        ! Search for this directory in the parent's file list
        ui_state%current_selection = 1  ! Default to first item
        
        if (associated(parent_node)) then
            do i = 1, parent_node%file_count
                if (parent_node%files(i)%is_directory .and. &
                    trim(parent_node%files(i)%name) == trim(child_name)) then
                    ui_state%current_selection = i
                    exit
                end if
            end do
            
            ! Ensure selection is valid
            if (ui_state%current_selection < 1) then
                ui_state%current_selection = 1
            end if
            if (ui_state%current_selection > parent_node%file_count) then
                ui_state%current_selection = parent_node%file_count
            end if
        end if
        
    end subroutine find_directory_in_parent

    ! Fast scrolling functions for smooth navigation
    subroutine handle_page_up()
        integer :: page_size, new_selection
        
        if (.not. associated(current_node)) return
        if (current_node%file_count <= 0) return
        
        page_size = max(1, (SCREEN_HEIGHT - 8) / 2)  ! Half screen for smoother scrolling
        new_selection = max(1, ui_state%current_selection - page_size)
        
        ui_state%current_selection = new_selection
    end subroutine handle_page_up

    subroutine handle_page_down()
        integer :: page_size, new_selection
        
        if (.not. associated(current_node)) return
        if (current_node%file_count <= 0) return
        
        page_size = max(1, (SCREEN_HEIGHT - 8) / 2)  ! Half screen for smoother scrolling
        new_selection = min(current_node%file_count, ui_state%current_selection + page_size)
        
        ui_state%current_selection = new_selection
    end subroutine handle_page_down

    subroutine handle_jump_to_top()
        if (.not. associated(current_node)) return
        
        ui_state%current_selection = 1
        ui_state%scroll_position = 0
    end subroutine handle_jump_to_top

    subroutine handle_jump_to_bottom()
        if (.not. associated(current_node)) return
        if (current_node%file_count <= 0) return
        
        ui_state%current_selection = current_node%file_count
        ui_state%scroll_position = max(0, current_node%file_count - (SCREEN_HEIGHT - 8))
    end subroutine handle_jump_to_bottom

end module interface_manager
