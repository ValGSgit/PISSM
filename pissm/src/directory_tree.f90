module directory_tree
    use constants_mod
    use types_mod
    use file_operations
    use utilities
    implicit none
    
contains
    
    subroutine create_directory_node(path, node)
        character(len=*), intent(in) :: path
        type(directory_node), pointer, intent(out) :: node
        type(file_entry) :: temp_files(MAX_FILES)
        integer :: actual_count
        
        allocate(node)
        node%path = trim(path)
        node%file_count = 0
        node%expanded = .false.
        node%child_count = 0
        
        ! First scan into temporary array to get actual count
        call scan_directory(path, temp_files, actual_count, .false.)
        
        ! Allocate only what we need (minimum 1 to avoid zero-size arrays)
        if (actual_count > 0) then
            allocate(node%files(actual_count))
            node%files(1:actual_count) = temp_files(1:actual_count)
            node%file_count = actual_count
        else
            allocate(node%files(1))  ! Allocate at least 1 element
            node%file_count = 0
        end if
    end subroutine create_directory_node
    
    subroutine refresh_directory_node(node, show_hidden)
        type(directory_node), pointer, intent(inout) :: node
        logical, intent(in) :: show_hidden
        type(file_entry) :: temp_files(MAX_FILES)
        integer :: actual_count
        
        if (associated(node)) then
            ! Scan into temporary array first
            call scan_directory(node%path, temp_files, actual_count, show_hidden)
            
            ! Reallocate if necessary
            if (allocated(node%files)) deallocate(node%files)
            
            if (actual_count > 0) then
                allocate(node%files(actual_count))
                node%files(1:actual_count) = temp_files(1:actual_count)
                node%file_count = actual_count
            else
                allocate(node%files(1))
                node%file_count = 0
            end if
        end if
    end subroutine refresh_directory_node
    
    subroutine display_directory_tree(node, start_line, max_lines, selection)
        type(directory_node), pointer, intent(in) :: node
        integer, intent(in) :: start_line, max_lines, selection
        
        integer :: i, current_line, display_line
        character(len=20) :: size_str
        character(len=8) :: icon
        character(len=30) :: name_color, size_color
        
        if (.not. associated(node)) return
        
        current_line = start_line
        display_line = 8  ! Start after enhanced header and column headers
        
        do i = 1, node%file_count
            if (current_line >= start_line .and. display_line <= max_lines) then
                call set_cursor_position(display_line, 1)
                
                ! Clear line with background color for selection
                if (i == selection) then
                    call print_colored(repeat(' ', SCREEN_WIDTH), COLOR_BLUE)
                    call set_cursor_position(display_line, 1)
                else
                    write(*, '(A)', advance='no') repeat(' ', SCREEN_WIDTH)
                    call set_cursor_position(display_line, 1)
                end if
                
                ! Selection indicator
                if (i == selection) then
                    call print_colored('►', COLOR_BRIGHT_YELLOW//COLOR_BOLD)
                else
                    write(*, '(A)', advance='no') ' '
                end if
                
                ! Get file icon and colors
                icon = get_file_icon(node%files(i)%name, node%files(i)%is_directory, &
                                   node%files(i)%permissions)
                
                ! Set colors based on file type and permissions
                if (node%files(i)%is_directory) then
                    name_color = COLOR_BRIGHT_CYAN//COLOR_BOLD
                    size_color = COLOR_DIM
                else if (node%files(i)%permissions(4:4) == 'x' .or. &
                        node%files(i)%permissions(7:7) == 'x' .or. &
                        node%files(i)%permissions(10:10) == 'x') then
                    name_color = COLOR_BRIGHT_GREEN//COLOR_BOLD
                    size_color = COLOR_GREEN
                else
                    name_color = COLOR_WHITE
                    size_color = COLOR_MAGENTA
                end if
                
                ! File type icon
                call set_cursor_position(display_line, 3)
                call print_colored(icon, COLOR_YELLOW)
                
                ! File name (truncate if too long)
                call set_cursor_position(display_line, 8)
                if (len_trim(node%files(i)%name) > 35) then
                    call print_colored(node%files(i)%name(1:32)//'...', name_color)
                else
                    call print_colored(trim(node%files(i)%name), name_color)
                end if
                
                ! File size
                if (.not. node%files(i)%is_directory .and. node%files(i)%size >= 0) then
                    call format_size(node%files(i)%size, size_str)
                    call set_cursor_position(display_line, 45)
                    call print_colored(adjustr(size_str(1:10)), size_color)
                end if
                
                ! Permissions with color coding
                call set_cursor_position(display_line, 55)
                call print_permissions_colored(node%files(i)%permissions)
                
                ! Owner and group
                call set_cursor_position(display_line, 68)
                if (len_trim(node%files(i)%owner_group) > 15) then
                    call print_colored(node%files(i)%owner_group(1:12)//'...', COLOR_CYAN)
                else
                    call print_colored(trim(node%files(i)%owner_group), COLOR_CYAN)
                end if
                
                ! Modification time
                call set_cursor_position(display_line, 85)
                call print_colored(trim(node%files(i)%modified_time), COLOR_BRIGHT_MAGENTA)
                
                display_line = display_line + 1
            end if
            current_line = current_line + 1
        end do
        
        ! Clear remaining lines
        do i = display_line, max_lines
            call set_cursor_position(i, 1)
            write(*, '(A)') repeat(' ', SCREEN_WIDTH)
        end do
    end subroutine display_directory_tree
    
    subroutine print_permissions_colored(permissions)
        character(len=*), intent(in) :: permissions
        integer :: i
        character :: perm_char
        
        do i = 1, min(len_trim(permissions), 10)
            perm_char = permissions(i:i)
            
            select case (i)
                case (1)  ! File type
                    select case (perm_char)
                        case ('d')
                            call print_colored(perm_char, COLOR_BLUE//COLOR_BOLD)
                        case ('l')
                            call print_colored(perm_char, COLOR_CYAN//COLOR_BOLD)
                        case ('-')
                            call print_colored(perm_char, COLOR_WHITE)
                        case default
                            call print_colored(perm_char, COLOR_YELLOW)
                    end select
                    
                case (2, 3, 4)  ! Owner permissions
                    if (perm_char == 'r' .or. perm_char == 'w' .or. perm_char == 'x') then
                        call print_colored(perm_char, COLOR_GREEN//COLOR_BOLD)
                    else
                        call print_colored(perm_char, COLOR_DIM)
                    end if
                    
                case (5, 6, 7)  ! Group permissions
                    if (perm_char == 'r' .or. perm_char == 'w' .or. perm_char == 'x') then
                        call print_colored(perm_char, COLOR_YELLOW)
                    else
                        call print_colored(perm_char, COLOR_DIM)
                    end if
                    
                case (8, 9, 10)  ! Other permissions
                    if (perm_char == 'r' .or. perm_char == 'w' .or. perm_char == 'x') then
                        call print_colored(perm_char, COLOR_RED)
                    else
                        call print_colored(perm_char, COLOR_DIM)
                    end if
            end select
        end do
    end subroutine print_permissions_colored
    
    function get_selected_file_path(node, selection) result(file_path)
        type(directory_node), pointer, intent(in) :: node
        integer, intent(in) :: selection
        character(len=MAX_PATH_LENGTH) :: file_path
        
        file_path = ''
        if (associated(node) .and. selection > 0 .and. selection <= node%file_count) then
            file_path = node%files(selection)%full_path
        end if
    end function get_selected_file_path
    
    function is_selected_directory(node, selection) result(is_dir)
        type(directory_node), pointer, intent(in) :: node
        integer, intent(in) :: selection
        logical :: is_dir
        
        is_dir = .false.
        if (associated(node) .and. selection > 0 .and. selection <= node%file_count) then
            is_dir = node%files(selection)%is_directory
        end if
    end function is_selected_directory
    
    recursive subroutine cleanup_directory_tree(node)
        type(directory_node), pointer, intent(inout) :: node
        
        if (associated(node)) then
            ! Note: children array is not implemented in this version
            ! Future enhancement could implement directory tree structure
            
            if (allocated(node%files)) then
                deallocate(node%files)
            end if
            
            deallocate(node)
            nullify(node)
        end if
    end subroutine cleanup_directory_tree
    
end module directory_tree