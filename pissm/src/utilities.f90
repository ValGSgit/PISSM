module utilities
    use constants_mod
    implicit none
    
contains
    
    subroutine clear_screen()
        write(*,'(A)') char(27)//'[2J'//char(27)//'[H'
    end subroutine clear_screen
    
    subroutine set_cursor_position(row, col)
        integer, intent(in) :: row, col
        write(*,'(A,I0,A,I0,A)', advance='no') char(27)//'[', row, ';', col, 'H'
    end subroutine set_cursor_position
    
    subroutine print_colored(text, color)
        character(len=*), intent(in) :: text, color
        write(*,'(A)', advance='no') trim(color)//trim(text)//COLOR_RESET
    end subroutine print_colored
    
    subroutine print_header()
        call clear_screen()
        call print_colored(repeat('=', SCREEN_WIDTH), COLOR_CYAN)
        write(*,*)
        call print_colored(' PISSM - Personal Interface System Structure & Modifications ', COLOR_BOLD//COLOR_WHITE)
        write(*,*)
        call print_colored(repeat('=', SCREEN_WIDTH), COLOR_CYAN)
        write(*,*)
    end subroutine print_header
    
    subroutine print_footer(message)
        character(len=*), intent(in), optional :: message
        integer :: row
        
        row = SCREEN_HEIGHT - 2
        call set_cursor_position(row, 1)
        call print_colored(repeat('-', SCREEN_WIDTH), COLOR_BLUE)
        
        if (present(message)) then
            call set_cursor_position(row + 1, 1)
            call print_colored(trim(message), COLOR_YELLOW)
        end if
    end subroutine print_footer
    
    subroutine print_enhanced_footer(message)
        character(len=*), intent(in), optional :: message
        integer :: row
        
        row = SCREEN_HEIGHT - 3
        call set_cursor_position(row, 1)
        call print_colored(repeat('═', SCREEN_WIDTH), COLOR_BRIGHT_BLUE)
        
        if (present(message)) then
            call set_cursor_position(row + 1, 1)
            call print_colored('│ '//trim(message), COLOR_BRIGHT_YELLOW)
            call set_cursor_position(row + 1, SCREEN_WIDTH)
            call print_colored('│', COLOR_BRIGHT_BLUE)
        end if
        
        call set_cursor_position(row + 2, 1)
        call print_colored(repeat('═', SCREEN_WIDTH), COLOR_BRIGHT_BLUE)
    end subroutine print_enhanced_footer
    
    function get_file_size(filename) result(size)
        character(len=*), intent(in) :: filename
        integer :: size, unit_num, iostat
        
        size = 0
        open(newunit=unit_num, file=trim(filename), status='old', &
             access='stream', iostat=iostat)
        
        if (iostat == 0) then
            inquire(unit=unit_num, size=size)
            close(unit_num)
        end if
    end function get_file_size
    
    function file_exists(filename) result(exists)
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=trim(filename), exist=exists)
    end function file_exists
    
    function is_directory(path) result(is_dir)
        character(len=*), intent(in) :: path
        logical :: is_dir
        character(len=512) :: command
        integer :: status
        
        ! Use system command to check if directory
        command = 'test -d "'//trim(path)//'"'
        call execute_command_line(command, exitstat=status)
        is_dir = (status == 0)
    end function is_directory
    
    subroutine format_size(size_bytes, formatted_size)
        integer(kind=8), intent(in) :: size_bytes
        character(len=*), intent(out) :: formatted_size
        real(kind=8) :: size_real
        
        size_real = real(size_bytes, kind=8)
        
        if (size_bytes < 1024_8) then
            write(formatted_size, '(I0,A)') size_bytes, ' B'
        else if (size_bytes < 1024_8*1024_8) then
            write(formatted_size, '(F6.1,A)') size_real/1024.0_8, ' KB'
        else if (size_bytes < 1024_8*1024_8*1024_8) then
            write(formatted_size, '(F6.1,A)') size_real/(1024.0_8*1024.0_8), ' MB'
        else if (size_bytes < 1024_8*1024_8*1024_8*1024_8) then
            write(formatted_size, '(F6.1,A)') size_real/(1024.0_8*1024.0_8*1024.0_8), ' GB'
        else
            write(formatted_size, '(F6.1,A)') size_real/(1024.0_8*1024.0_8*1024.0_8*1024.0_8), ' TB'
        end if
    end subroutine format_size
    
    subroutine get_current_time(time_str)
        character(len=*), intent(out) :: time_str
        character(len=8) :: date
        character(len=10) :: time
        
        call date_and_time(date, time)
        time_str = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '// &
                  time(1:2)//':'//time(3:4)
    end subroutine get_current_time
    
    function get_file_icon(filename, is_directory, permissions) result(icon)
        character(len=*), intent(in) :: filename, permissions
        logical, intent(in) :: is_directory
        character(len=4) :: icon
        character(len=10) :: extension
        integer :: dot_pos
        
        if (is_directory) then
            icon = '📁'
            return
        end if
        
        ! Check if executable
        if (permissions(4:4) == 'x' .or. permissions(7:7) == 'x' .or. permissions(10:10) == 'x') then
            icon = '⚡'
            return
        end if
        
        ! Get file extension
        dot_pos = index(filename, '.', .true.)
        if (dot_pos > 0) then
            extension = filename(dot_pos+1:)
            call to_lowercase(extension)
            
            select case (trim(extension))
                case ('txt', 'md', 'rst', 'log')
                    icon = '📝'
                case ('jpg', 'jpeg', 'png', 'gif', 'bmp', 'svg')
                    icon = '🖼️'
                case ('zip', 'tar', 'gz', 'bz2', 'xz', '7z', 'rar')
                    icon = '📦'
                case ('c', 'cpp', 'h', 'hpp', 'f90', 'f95', 'py', 'js', 'html', 'css')
                    icon = '💾'
                case default
                    icon = '📄'
            end select
        else
            icon = '📄'
        end if
    end function get_file_icon
    
    subroutine to_lowercase(string)
        character(len=*), intent(inout) :: string
        integer :: i, char_code
        
        do i = 1, len_trim(string)
            char_code = iachar(string(i:i))
            if (char_code >= 65 .and. char_code <= 90) then
                string(i:i) = achar(char_code + 32)
            end if
        end do
    end subroutine to_lowercase
    
    subroutine draw_breadcrumb(path)
        character(len=*), intent(in) :: path
        character(len=MAX_PATH_LENGTH) :: temp_path
        character(len=50) :: part
        integer :: i, start_pos, slash_pos
        
        call set_cursor_position(3, 1)
        call print_colored('📍 ', COLOR_YELLOW)
        
        temp_path = trim(path)
        if (temp_path == '/') then
            call print_colored('/', COLOR_BRIGHT_CYAN//COLOR_BOLD)
            return
        end if
        
        start_pos = 1
        if (temp_path(1:1) == '/') start_pos = 2
        
        call print_colored('/', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        
        do while (start_pos <= len_trim(temp_path))
            slash_pos = index(temp_path(start_pos:), '/')
            if (slash_pos == 0) then
                part = temp_path(start_pos:)
                start_pos = len_trim(temp_path) + 1
            else
                part = temp_path(start_pos:start_pos+slash_pos-2)
                start_pos = start_pos + slash_pos
            end if
            
            if (len_trim(part) > 0) then
                call print_colored(trim(part), COLOR_BRIGHT_CYAN//COLOR_BOLD)
                if (start_pos <= len_trim(temp_path)) then
                    call print_colored(' › ', COLOR_YELLOW)
                end if
            end if
        end do
    end subroutine draw_breadcrumb
    
    subroutine print_header_enhanced()
        call clear_screen()
        
        ! Top border with gradient effect
        call set_cursor_position(1, 1)
        call print_colored(repeat('═', SCREEN_WIDTH), COLOR_BRIGHT_CYAN//COLOR_BOLD)
        
        ! Title line
        call set_cursor_position(2, 1)
        call print_colored('║', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        call set_cursor_position(2, (SCREEN_WIDTH - 60) / 2)
        call print_colored('✨ PISSM - Personal Interface System Structure & Modifications ✨', COLOR_BRIGHT_YELLOW//COLOR_BOLD)
        call set_cursor_position(2, SCREEN_WIDTH)
        call print_colored('║', COLOR_BRIGHT_CYAN//COLOR_BOLD)
        
        ! Bottom border
        call set_cursor_position(4, 1)
        call print_colored(repeat('═', SCREEN_WIDTH), COLOR_BRIGHT_CYAN//COLOR_BOLD)
    end subroutine print_header_enhanced
    
    subroutine print_column_headers()
        call set_cursor_position(6, 1)
        call print_colored('  ', COLOR_WHITE)
        call print_colored('Type', COLOR_BOLD//COLOR_WHITE)
        call set_cursor_position(6, 8)
        call print_colored('Name', COLOR_BOLD//COLOR_WHITE)
        call set_cursor_position(6, 45)
        call print_colored('Size', COLOR_BOLD//COLOR_WHITE)
        call set_cursor_position(6, 55)
        call print_colored('Permissions', COLOR_BOLD//COLOR_WHITE)
        call set_cursor_position(6, 68)
        call print_colored('Owner.Group', COLOR_BOLD//COLOR_WHITE)
        call set_cursor_position(6, 85)
        call print_colored('Modified', COLOR_BOLD//COLOR_WHITE)
        
        call set_cursor_position(7, 1)
        call print_colored(repeat('─', SCREEN_WIDTH-1), COLOR_DIM)
    end subroutine print_column_headers

end module utilities