!> \brief Utility Functions Module
!!
!! Provides general-purpose utility functions for:
!!  - Terminal control and ANSI escape sequences
!!  - File information and size formatting
!!  - String manipulation and case conversion
!!  - Display formatting and UI elements
!!
!! \author Your Name
!! \date 2024

module utilities
    use constants_mod
    implicit none
    
    private
    
    !> Public subroutines
    public :: clear_screen, set_cursor_position, get_terminal_size
    public :: clear_line, print_colored, print_header, print_footer
    public :: print_enhanced_footer, get_file_size, file_exists
    public :: is_directory, format_size, get_current_time
    public :: get_file_icon, to_lowercase, draw_breadcrumb
    public :: print_header_enhanced, print_column_headers
    public :: setup_terminal, restore_terminal
    
contains
    
    !> Clear the entire screen and move cursor to top-left
    subroutine clear_screen()
        ! Clear screen using standard ANSI escape sequence
        write(*,'(A)', advance='no') char(27)//'[2J'//char(27)//'[H'
    end subroutine clear_screen
    
    !> Set cursor to specified row and column
    subroutine set_cursor_position(row, col)
        integer, intent(in) :: row   !! Row number (1-based)
        integer, intent(in) :: col   !! Column number (1-based)
        write(*,'(A,I0,A,I0,A)', advance='no') char(27)//'[', row, ';', col, 'H'
    end subroutine set_cursor_position

    !> Detect current terminal dimensions using tput
    !!
    !! Attempts to get actual terminal size using tput command.
    !! Falls back to defaults (80x24) if detection fails.
    subroutine get_terminal_size(width, height)
        integer, intent(out) :: width    !! Terminal width in characters
        integer, intent(out) :: height   !! Terminal height in characters
        integer :: iostat_w, iostat_h, temp_width, temp_height
        integer :: unit_num
        
        ! Set default values in case detection fails
        width = 80
        height = 24
        
        ! Try to get actual terminal size using tput command
        call execute_command_line('tput cols > /tmp/pissm_cols 2>/dev/null')
        call execute_command_line('tput lines > /tmp/pissm_lines 2>/dev/null')
        
        ! Read width
        open(newunit=unit_num, file='/tmp/pissm_cols', status='old', iostat=iostat_w)
        if (iostat_w == 0) then
            read(unit_num, *, iostat=iostat_w) temp_width
            close(unit_num)
            if (iostat_w == 0 .and. temp_width > 20 .and. temp_width < 500) then
                width = temp_width
            end if
        end if
        
        ! Read height  
        open(newunit=unit_num, file='/tmp/pissm_lines', status='old', iostat=iostat_h)
        if (iostat_h == 0) then
            read(unit_num, *, iostat=iostat_h) temp_height
            close(unit_num)
            if (iostat_h == 0 .and. temp_height > 10 .and. temp_height < 200) then
                height = temp_height
            end if
        end if
        
        ! Clean up temp files
        call execute_command_line('rm -f /tmp/pissm_cols /tmp/pissm_lines 2>/dev/null')
        
    end subroutine get_terminal_size
    
    !> Clear from cursor to end of current line
    subroutine clear_line()
        write(*,'(A)', advance='no') char(27)//'[K'
    end subroutine clear_line
    
    !> Print text with ANSI color code
    subroutine print_colored(text, color)
        character(len=*), intent(in) :: text    !! Text to print
        character(len=*), intent(in) :: color   !! ANSI color code
        write(*,'(A)', advance='no') trim(color)//trim(text)//COLOR_RESET
    end subroutine print_colored
    
    !> Print page header
    subroutine print_header()
        call clear_screen()
        call print_colored(repeat('=', SCREEN_WIDTH), COLOR_CYAN)
        write(*,*)
        call print_colored(' PISSM - Personal Interface System Structure & Modifications ', COLOR_BOLD//COLOR_WHITE)
        write(*,*)
        call print_colored(repeat('=', SCREEN_WIDTH), COLOR_CYAN)
    end subroutine print_header
    
    !> Print page footer with optional message
    subroutine print_footer(message)
        character(len=*), intent(in), optional :: message  !! Footer message
        integer :: row
        
        row = SCREEN_HEIGHT - 2
        call set_cursor_position(row, 1)
        call print_colored(repeat('-', SCREEN_WIDTH), COLOR_BLUE)
        
        if (present(message)) then
            call set_cursor_position(row + 1, 1)
            call print_colored(trim(message), COLOR_YELLOW)
        end if
    end subroutine print_footer
    
    !> Print enhanced footer with box drawing
    subroutine print_enhanced_footer(message)
        character(len=*), intent(in), optional :: message  !! Footer message
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
    
    !> Get file size in bytes
    !!
    !! Attempts to open file and determine its size using INQUIRE.
    !! Returns 0 if file cannot be accessed.
    function get_file_size(filename) result(size)
        character(len=*), intent(in) :: filename  !! File path
        integer :: size                           !! File size in bytes
        integer :: unit_num, iostat
        
        size = 0
        open(newunit=unit_num, file=trim(filename), status='old', &
             access='stream', iostat=iostat)
        
        if (iostat == 0) then
            inquire(unit=unit_num, size=size)
            close(unit_num)
        end if
    end function get_file_size
    
    !> Check if file exists
    function file_exists(filename) result(exists)
        character(len=*), intent(in) :: filename  !! File path
        logical :: exists                         !! True if file exists
        
        inquire(file=trim(filename), exist=exists)
    end function file_exists
    
    !> Check if path is a directory
    function is_directory(path) result(is_dir)
        character(len=*), intent(in) :: path   !! Directory path
        logical :: is_dir                      !! True if path is a directory
        character(len=512) :: command
        integer :: status
        
        ! Use system command to check if directory
        command = 'test -d "'//trim(path)//'"'
        call execute_command_line(command, exitstat=status)
        is_dir = (status == 0)
    end function is_directory
    
    !> Format byte size as human-readable string
    !!
    !! Converts bytes to KB, MB, GB, TB with appropriate precision.
    subroutine format_size(size_bytes, formatted_size)
        integer(kind=8), intent(in) :: size_bytes        !! Size in bytes
        character(len=*), intent(out) :: formatted_size  !! Formatted size string
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
    
    !> Get current date and time as formatted string
    subroutine get_current_time(time_str)
        character(len=*), intent(out) :: time_str  !! Formatted date-time string
        character(len=8) :: date
        character(len=10) :: time
        
        call date_and_time(date, time)
        time_str = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '// &
                  time(1:2)//':'//time(3:4)
    end subroutine get_current_time
    
    !> Get appropriate emoji icon for file type
    !!
    !! Returns an emoji based on file type, permissions, or extension.
    function get_file_icon(filename, is_directory, permissions) result(icon)
        character(len=*), intent(in) :: filename       !! File name
        logical, intent(in) :: is_directory            !! Directory flag
        character(len=*), intent(in) :: permissions    !! Permissions string
        character(len=8) :: icon                       !! Emoji icon
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
    
    !> Convert string to lowercase
    subroutine to_lowercase(string)
        character(len=*), intent(inout) :: string   !! String to convert
        integer :: i, char_code
        
        do i = 1, len_trim(string)
            char_code = iachar(string(i:i))
            if (char_code >= 65 .and. char_code <= 90) then
                string(i:i) = achar(char_code + 32)
            end if
        end do
    end subroutine to_lowercase
    
    !> Draw breadcrumb navigation path
    subroutine draw_breadcrumb(path)
        character(len=*), intent(in) :: path    !! Directory path
        character(len=MAX_PATH_LENGTH) :: temp_path
        character(len=50) :: part
        integer :: start_pos, slash_pos
        
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
    
    !> Print enhanced page header with styled title
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
    
    !> Print column headers for file listing
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
    
    !> Setup terminal for raw input mode
    !!
    !! Configures terminal for interactive input with hidden cursor.
    subroutine setup_terminal()
        ! Set terminal to raw mode for better input handling
        call execute_command_line('stty raw -echo 2>/dev/null')
        
        ! Hide cursor for cleaner interface
        write(*,'(A)', advance='no') char(27)//'[?25l'
        
        ! Enable alternative screen buffer
        write(*,'(A)', advance='no') char(27)//'[?1049h'
    end subroutine setup_terminal
    
    !> Restore terminal to normal state
    !!
    !! Resets terminal settings and shows cursor.
    subroutine restore_terminal()
        ! Show cursor
        write(*,'(A)', advance='no') char(27)//'[?25h'
        
        ! Disable alternative screen buffer
        write(*,'(A)', advance='no') char(27)//'[?1049l'
        
        ! Restore terminal to normal state
        call execute_command_line('stty sane 2>/dev/null')
        call execute_command_line('reset 2>/dev/null')
    end subroutine restore_terminal

end module utilities