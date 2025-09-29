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
        integer :: i
        
        ! Get current working directory
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
        
        ui_state%current_path = trim(pwd)
        original_path = ui_state%current_path
        
        ! Initialize UI state
        ui_state%current_selection = 1
        ui_state%scroll_position = 0
        ui_state%view_mode = 1
        ui_state%show_hidden = .false.
        ui_state%clipboard_path = ''
        ui_state%clipboard_operation = 0
        
        ! Create initial directory node
        call create_directory_node(ui_state%current_path, current_node)
        if (associated(current_node)) then
            root_node => current_node
            call refresh_directory_node(current_node, ui_state%show_hidden)
        else
            write(*,*) 'Error: Cannot access initial directory'
            stop 1
        end if
    end subroutine initialize_interface
    
    subroutine main_interface_loop()
        character :: key
        logical :: need_refresh
        
        do while (program_running)
            call display_main_interface()
            
            need_refresh = .false.
            key = get_key()
            
            select case (key)
                case ('q', 'Q')
                    program_running = .false.
                    
                case ('j', 'J')  ! Down
                    if (ui_state%current_selection < current_node%file_count) then
                        ui_state%current_selection = ui_state%current_selection + 1
                        call adjust_scroll_position()
                        need_refresh = .true.
                    end if
                    
                case ('k', 'K')  ! Up
                    if (ui_state%current_selection > 1) then
                        ui_state%current_selection = ui_state%current_selection - 1
                        call adjust_scroll_position()
                        need_refresh = .true.
                    end if
                    
                case ('g', 'G')  ! Go to first/last
                    call handle_goto_operation()
                    need_refresh = .true.
                    
                case ('h', 'H')  ! Toggle hidden files
                    ui_state%show_hidden = .not. ui_state%show_hidden
                    call refresh_directory_node(current_node, ui_state%show_hidden)
                    ui_state%current_selection = 1
                    ui_state%scroll_position = 0
                    need_refresh = .true.
                    
                case ('r', 'R')  ! Refresh
                    call refresh_directory_node(current_node, ui_state%show_hidden)
                    need_refresh = .true.
                    
                case (char(10), char(13))  ! Enter
                    call handle_enter_key()
                    need_refresh = .true.
                    
                case ('c', 'C')  ! Copy
                    call handle_copy_operation()
                    
                case ('m', 'M')  ! Move
                    call handle_move_operation()
                    
                case ('d', 'D')  ! Delete
                    call handle_delete_operation()
                    need_refresh = .true.
                    
                case ('n', 'N')  ! New file/directory
                    call handle_create_operation()
                    need_refresh = .true.
                    
                case ('s', 'S')  ! Search
                    call handle_search_operation()
                    
                case ('p', 'P')  ! Paste
                    call handle_paste_operation()
                    need_refresh = .true.
                    
                case ('b', 'B')  ! Back/Up directory
                    call handle_back_operation()
                    need_refresh = .true.
                    
                case ('i', 'I')  ! Info/Details view
                    call show_file_details()
                    need_refresh = .true.
                    
                case ('/', '\')  ! Quick search
                    call handle_quick_search()
                    need_refresh = .true.
                    
                case ('~')  ! Go to home directory
                    call handle_home_directory()
                    need_refresh = .true.
                    
                case ('?')  ! Help
                    call show_help()
                    need_refresh = .true.
                    
                case default
                    ! Ignore unknown keys
            end select
            
            if (need_refresh) then
                call display_main_interface()
            end if
        end do
    end subroutine main_interface_loop
    
    subroutine display_main_interface()
        character(len=200) :: status_line
        integer :: max_display_lines
        
        call print_header_enhanced()
        call draw_breadcrumb(ui_state%current_path)
        call print_column_headers()
        
        ! Display file listing
        max_display_lines = SCREEN_HEIGHT - 12
        call display_directory_tree(current_node, ui_state%scroll_position, &
                                  max_display_lines, ui_state%current_selection)
        
        ! Enhanced status line
        write(status_line, '(A,I0,A,I0,A)') '📊 Files: ', current_node%file_count, &
               ' │ Selected: ', ui_state%current_selection, ' │ '
        
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
        call print_colored('⬆️⬇️ j/k:Move │ ⏎ Enter:Open │ 📋 c:Copy │ ✂️  m:Move │ 🗑️  d:Delete │ ➕ n:New │ 🔍 s:Search │ 📌 p:Paste │ ⬅️ b:Back │ 👁️  h:Hidden │ 🔄 r:Refresh │ ❌ q:Quit │ ❓ ?:Help', COLOR_BRIGHT_WHITE)
    end subroutine display_main_interface
    
    subroutine handle_enter_key()
        character(len=MAX_PATH_LENGTH) :: selected_path
        type(directory_node), pointer :: new_node
        
        if (current_node%file_count == 0) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        
        if (is_selected_directory(current_node, ui_state%current_selection)) then
            ! Navigate to directory
            call create_directory_node(selected_path, new_node)
            if (associated(new_node)) then
                ! Clean up current node to prevent memory leaks
                call cleanup_directory_tree(current_node)
                
                current_node => new_node
                ui_state%current_path = selected_path
                ui_state%current_selection = 1
                ui_state%scroll_position = 0
                
                call refresh_directory_node(current_node, ui_state%show_hidden)
            else
                call show_message('Error: Cannot access directory - '//trim(selected_path))
            end if
        else
            ! Try to open file with default program
            call open_file(selected_path)
            call show_message('Opening file: '//trim(current_node%files(ui_state%current_selection)%name))
        end if
    end subroutine handle_enter_key
    
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
        
        call show_message('File marked for move: '//trim(selected_path))
    end subroutine handle_move_operation
    
    subroutine handle_delete_operation()
        character(len=MAX_PATH_LENGTH) :: selected_path
        character(len=200) :: confirmation_msg
        logical :: success
        
        if (current_node%file_count == 0) return
        
        selected_path = get_selected_file_path(current_node, ui_state%current_selection)
        
        write(confirmation_msg, '(A)') 'Delete '//trim(selected_path)//'?'
        
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
    
    subroutine handle_create_operation()
        character(len=MAX_PATH_LENGTH) :: new_name, full_path
        character :: file_type
        logical :: success
        
        write(*, '(A)', advance='no') 'Create (f)ile or (d)irectory? '
        read(*, '(A)') file_type
        
        if (file_type /= 'f' .and. file_type /= 'd' .and. &
            file_type /= 'F' .and. file_type /= 'D') then
            call show_message('Invalid selection')
            return
        end if
        
        call get_string('Enter name: ', new_name, MAX_FILENAME)
        if (len_trim(new_name) == 0) then
            call show_message('Invalid name')
            return
        end if
        
        full_path = trim(ui_state%current_path)//'/'//trim(new_name)
        
        if (file_type == 'f' .or. file_type == 'F') then
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
    end subroutine handle_create_operation
    
    subroutine handle_search_operation()
        character(len=MAX_FILENAME) :: search_pattern
        type(file_entry) :: search_results(MAX_FILES)
        integer :: result_count, i, selection
        character :: key
        
        call get_string('Search pattern: ', search_pattern, MAX_FILENAME)
        if (len_trim(search_pattern) == 0) return
        
        call search_files(ui_state%current_path, search_pattern, search_results, result_count)
        
        if (result_count == 0) then
            call show_message('No files found matching pattern')
            return
        end if
        
        ! Display search results with enhanced interface
        selection = 1
        do
            call print_header_enhanced()
            
            call set_cursor_position(6, 1)
            call print_colored('🔍 Search Results', COLOR_BOLD//COLOR_BRIGHT_CYAN)
            call set_cursor_position(6, 25)
            write(*, '(A,I0,A,A,A)', advance='no') '(', result_count, ' files found matching "', trim(search_pattern), '")'
            
            call set_cursor_position(7, 1)
            call print_colored(repeat('═', SCREEN_WIDTH), COLOR_CYAN)
            
            do i = 1, min(result_count, SCREEN_HEIGHT - 12)
                call set_cursor_position(8 + i, 1)
                
                if (i == selection) then
                    call print_colored(repeat(' ', SCREEN_WIDTH), COLOR_BLUE)
                    call set_cursor_position(8 + i, 1)
                    call print_colored('►', COLOR_BRIGHT_YELLOW//COLOR_BOLD)
                else
                    call print_colored(' ', COLOR_WHITE)
                end if
                
                call set_cursor_position(8 + i, 3)
                if (search_results(i)%is_directory) then
                    call print_colored('📁', COLOR_YELLOW)
                else
                    call print_colored('📄', COLOR_WHITE)
                end if
                
                call set_cursor_position(8 + i, 6)
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
            end select
        end do
    end subroutine handle_search_operation
    
    subroutine handle_paste_operation()
        character(len=MAX_PATH_LENGTH) :: dest_path
        character(len=MAX_FILENAME) :: filename
        logical :: success
        
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
        integer :: pos
        
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
        
        ! Create new directory node for parent
        call create_directory_node(parent_path, parent_node)
        if (associated(parent_node)) then
            ! Clean up current node properly
            call cleanup_directory_tree(current_node)
            
            ! Update navigation state
            current_node => parent_node
            ui_state%current_path = parent_path
            ui_state%current_selection = 1
            ui_state%scroll_position = 0
            
            call refresh_directory_node(current_node, ui_state%show_hidden)
        else
            call show_message('Error: Cannot access parent directory')
        end if
    end subroutine handle_back_operation
    
    subroutine open_file(filepath)
        character(len=*), intent(in) :: filepath
        character(len=1024) :: command
        
        command = 'xdg-open "'//trim(filepath)//'" 2>/dev/null &'
        call execute_command_line(command)
    end subroutine open_file
    
    subroutine show_message(message)
        character(len=*), intent(in) :: message
        
        call set_cursor_position(SCREEN_HEIGHT - 3, 1)
        call print_colored(repeat(' ', SCREEN_WIDTH), COLOR_RESET)
        call set_cursor_position(SCREEN_HEIGHT - 3, 1)
        call print_colored(trim(message), COLOR_YELLOW//COLOR_BOLD)
        call wait_for_key()
    end subroutine show_message
    
    subroutine show_help()
        call clear_screen()
        call print_header_enhanced()
        
        call set_cursor_position(6, 1)
        call print_colored('🆘 PISSM Help - Keyboard Commands', COLOR_BOLD//COLOR_BRIGHT_YELLOW)
        call set_cursor_position(7, 1)
        call print_colored(repeat('═', 50), COLOR_YELLOW)
        
        call set_cursor_position(9, 1)
        call print_colored('📍 Navigation:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(10, 3)
        call print_colored('j/k or ↓/↑   ', COLOR_GREEN)
        call print_colored('- Move selection up/down', COLOR_WHITE)
        call set_cursor_position(11, 3)
        call print_colored('Enter        ', COLOR_GREEN)
        call print_colored('- Open file/directory', COLOR_WHITE)
        call set_cursor_position(12, 3)
        call print_colored('b            ', COLOR_GREEN)
        call print_colored('- Go back to parent directory', COLOR_WHITE)
        call set_cursor_position(13, 3)
        call print_colored('~            ', COLOR_GREEN)
        call print_colored('- Go to home directory', COLOR_WHITE)
        call set_cursor_position(14, 3)
        call print_colored('gg           ', COLOR_GREEN)
        call print_colored('- Go to first item', COLOR_WHITE)
        call set_cursor_position(15, 3)
        call print_colored('G            ', COLOR_GREEN)
        call print_colored('- Go to last item', COLOR_WHITE)
        
        call set_cursor_position(17, 1)
        call print_colored('🔍 Search & View:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(18, 3)
        call print_colored('/            ', COLOR_GREEN)
        call print_colored('- Quick search in current directory', COLOR_WHITE)
        call set_cursor_position(19, 3)
        call print_colored('s            ', COLOR_GREEN)
        call print_colored('- Advanced file search', COLOR_WHITE)
        call set_cursor_position(20, 3)
        call print_colored('i            ', COLOR_GREEN)
        call print_colored('- Show detailed file information', COLOR_WHITE)
        call set_cursor_position(21, 3)
        call print_colored('h            ', COLOR_GREEN)
        call print_colored('- Toggle hidden files visibility', COLOR_WHITE)
        call set_cursor_position(22, 3)
        call print_colored('r            ', COLOR_GREEN)
        call print_colored('- Refresh current directory', COLOR_WHITE)
        
        call set_cursor_position(24, 1)
        call print_colored('📁 File Operations:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(25, 3)
        call print_colored('c            ', COLOR_GREEN)
        call print_colored('- Copy selected file to clipboard', COLOR_WHITE)
        call set_cursor_position(26, 3)
        call print_colored('m            ', COLOR_GREEN)
        call print_colored('- Move selected file to clipboard', COLOR_WHITE)
        call set_cursor_position(27, 3)
        call print_colored('p            ', COLOR_GREEN)
        call print_colored('- Paste from clipboard', COLOR_WHITE)
        call set_cursor_position(28, 3)
        call print_colored('d            ', COLOR_GREEN)
        call print_colored('- Delete selected file/directory', COLOR_WHITE)
        call set_cursor_position(29, 3)
        call print_colored('n            ', COLOR_GREEN)
        call print_colored('- Create new file or directory', COLOR_WHITE)
        
        call set_cursor_position(31, 1)
        call print_colored('⚙️  System:', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(32, 3)
        call print_colored('?            ', COLOR_GREEN)
        call print_colored('- Show this help screen', COLOR_WHITE)
        call set_cursor_position(33, 3)
        call print_colored('q            ', COLOR_GREEN)
        call print_colored('- Quit PISSM', COLOR_WHITE)
        
        call set_cursor_position(35, 1)
        call print_colored('💡 Tips:', COLOR_BRIGHT_MAGENTA//COLOR_BOLD)
        call set_cursor_position(36, 3)
        call print_colored('• Use emoji indicators to identify file types quickly', COLOR_BRIGHT_WHITE)
        call set_cursor_position(37, 3)
        call print_colored('• Permissions are color-coded: Green=Owner, Yellow=Group, Red=Others', COLOR_BRIGHT_WHITE)
        call set_cursor_position(38, 3)
        call print_colored('• File sizes are automatically formatted (B, KB, MB, GB, TB)', COLOR_BRIGHT_WHITE)
        
        call print_enhanced_footer('')
        call wait_for_key()
    end subroutine show_help
    
    subroutine cleanup_interface()
        call cleanup_directory_tree(root_node)
        call clear_screen()
        call print_colored('Thanks for using PISSM!', COLOR_GREEN//COLOR_BOLD)
        write(*,*)
    end subroutine cleanup_interface
    
    subroutine adjust_scroll_position()
        integer :: max_visible_lines
        
        max_visible_lines = SCREEN_HEIGHT - 12
        
        ! Adjust scroll to keep selection visible
        if (ui_state%current_selection <= ui_state%scroll_position) then
            ui_state%scroll_position = max(0, ui_state%current_selection - 1)
        else if (ui_state%current_selection > ui_state%scroll_position + max_visible_lines) then
            ui_state%scroll_position = ui_state%current_selection - max_visible_lines
        end if
    end subroutine adjust_scroll_position
    
    subroutine handle_goto_operation()
        character :: second_key
        
        write(*, '(A)', advance='no') 'Go to: (g)top, (G)bottom: '
        second_key = get_key()
        
        select case (second_key)
            case ('g')  ! Go to top
                ui_state%current_selection = 1
                ui_state%scroll_position = 0
            case ('G')  ! Go to bottom
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
        call print_colored('📋 Detailed File Information', COLOR_BOLD//COLOR_BRIGHT_CYAN)
        call set_cursor_position(7, 1)
        call print_colored(repeat('═', 50), COLOR_CYAN)
        
        ! Basic information
        call set_cursor_position(9, 1)
        call print_colored('📄 File Name: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(current_node%files(ui_state%current_selection)%name), COLOR_WHITE)
        
        call set_cursor_position(10, 1)
        call print_colored('📁 Full Path: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(selected_path), COLOR_CYAN)
        
        call set_cursor_position(11, 1)
        call print_colored('🔐 Permissions: ', COLOR_BRIGHT_YELLOW)
        call print_permissions_colored(current_node%files(ui_state%current_selection)%permissions)
        
        call set_cursor_position(12, 1)
        call print_colored('👤 Owner/Group: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(current_node%files(ui_state%current_selection)%owner_group), COLOR_GREEN)
        
        if (.not. current_node%files(ui_state%current_selection)%is_directory) then
            call set_cursor_position(13, 1)
            call print_colored('📊 Size: ', COLOR_BRIGHT_YELLOW)
            call format_size(current_node%files(ui_state%current_selection)%size, line)
            call print_colored(trim(line), COLOR_MAGENTA)
        end if
        
        call set_cursor_position(14, 1)
        call print_colored('🕒 Modified: ', COLOR_BRIGHT_YELLOW)
        call print_colored(trim(current_node%files(ui_state%current_selection)%modified_time), COLOR_BRIGHT_MAGENTA)
        
        ! Get file type using 'file' command
        temp_file = '/tmp/pissm_file_info'
        command = 'file "'//trim(selected_path)//'" > '//trim(temp_file)//' 2>/dev/null'
        call execute_command_line(command)
        
        open(newunit=unit_num, file=temp_file, status='old', iostat=iostat)
        if (iostat == 0) then
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat == 0) then
                ! Extract file type (after first colon)
                iostat = index(line, ':')
                if (iostat > 0) then
                    file_type = trim(adjustl(line(iostat+1:)))
                    call set_cursor_position(15, 1)
                    call print_colored('🏷️  Type: ', COLOR_BRIGHT_YELLOW)
                    call print_colored(trim(file_type), COLOR_BRIGHT_GREEN)
                end if
            end if
            close(unit_num)
        end if
        
        ! Cleanup
        command = 'rm -f '//trim(temp_file)
        call execute_command_line(command)
        
        call set_cursor_position(SCREEN_HEIGHT - 2, 1)
        call print_colored('Press any key to return...', COLOR_YELLOW)
        call wait_for_key()
    end subroutine show_file_details
    
    subroutine handle_quick_search()
        character(len=MAX_FILENAME) :: search_pattern
        character(len=100) :: line
        integer :: i, match_count, first_match
        character(len=MAX_FILENAME) :: lowercase_name, lowercase_pattern
        
        call get_string('Quick search: ', search_pattern, MAX_FILENAME)
        if (len_trim(search_pattern) == 0) return
        
        ! Convert search pattern to lowercase
        lowercase_pattern = search_pattern
        call to_lowercase(lowercase_pattern)
        
        ! Find first match
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
            
            if (match_count > 1) then
                write(line, '(A,I0,A)') 'Found ', match_count, ' matches. Use n/N to navigate.'
                call show_message(trim(line))
            end if
        else
            call show_message('No matches found for: '//trim(search_pattern))
        end if
    end subroutine handle_quick_search
    
    subroutine handle_home_directory()
        character(len=MAX_PATH_LENGTH) :: home_path
        type(directory_node), pointer :: new_node
        
        call get_environment_variable('HOME', home_path)
        if (len_trim(home_path) == 0) then
            home_path = '/home/'//trim(get_username())
        end if
        
        call create_directory_node(home_path, new_node)
        if (associated(new_node)) then
            call cleanup_directory_tree(current_node)
            current_node => new_node
            root_node => new_node
            ui_state%current_path = home_path
            ui_state%current_selection = 1
            ui_state%scroll_position = 0
        end if
    end subroutine handle_home_directory
    
    function get_username() result(username)
        character(len=50) :: username
        call get_environment_variable('USER', username)
        if (len_trim(username) == 0) then
            call get_environment_variable('LOGNAME', username)
        end if
        if (len_trim(username) == 0) then
            username = 'user'
        end if
    end function get_username

end module interface_manager