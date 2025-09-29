module user_input
    use constants_mod
    use utilities
    implicit none
    
contains
    
    function get_key() result(key)
        character :: key
        character :: seq1, seq2
        integer :: iostat
        
        ! Initialize key to prevent undefined behavior
        key = ' '
        
        ! Read one character in raw mode with proper signal handling
        call execute_command_line('stty raw -echo min 1 time 0 intr ^C quit ^\ 2>/dev/null')
        read(*, '(A)', iostat=iostat, advance='no') key
        
        if (iostat /= 0) then
            call execute_command_line('stty sane 2>/dev/null')
            ! Check if this was an interrupt signal
            if (iostat < 0) then
                key = char(3)  ! Return Ctrl+C for clean exit
            else
                key = ' '
            end if
            return
        end if
        
        ! Handle special control characters immediately
        if (iachar(key) == 3 .or. iachar(key) == 4) then  ! Ctrl+C or Ctrl+D
            call execute_command_line('stty sane 2>/dev/null')
            return
        end if
        
        ! Check for escape sequences (arrow keys)
        if (iachar(key) == 27) then  ! ESC character detected
            ! Read the next two characters for arrow key sequence with timeout
            read(*, '(A)', iostat=iostat, advance='no') seq1
            if (iostat /= 0) then
                call execute_command_line('stty sane 2>/dev/null')
                key = ' '  ! Treat incomplete escape sequence as space
                return
            end if
            
            read(*, '(A)', iostat=iostat, advance='no') seq2
            if (iostat /= 0) then
                call execute_command_line('stty sane 2>/dev/null')
                key = ' '  ! Treat incomplete escape sequence as space
                return
            end if
            
            ! Restore terminal before processing
            call execute_command_line('stty sane 2>/dev/null')
            
            ! Check for standard arrow key sequence ESC[A, ESC[B, etc.
            if (seq1 == '[') then
                select case (seq2)
                    case ('A')  ! Up arrow
                        key = 'k'
                    case ('B')  ! Down arrow
                        key = 'j'
                    case ('C')  ! Right arrow
                        key = '>'
                    case ('D')  ! Left arrow
                        key = 'b'
                    case default
                        key = ' '  ! Unknown escape sequence
                end select
            else
                key = ' '  ! Not an arrow key sequence
            end if
        else
            ! Regular key, restore terminal
            call execute_command_line('stty sane 2>/dev/null')
        end if
    end function get_key
    
    subroutine get_string(prompt, input_string, max_length)
        character(len=*), intent(in) :: prompt
        character(len=*), intent(out) :: input_string
        integer, intent(in) :: max_length
        
        ! Position cursor at bottom of screen for input
        call set_cursor_position(SCREEN_HEIGHT, 1)
        call clear_line()
        write(*, '(A)', advance='no') trim(prompt)
        read(*, '(A)') input_string
        
        if (len_trim(input_string) > max_length) then
            input_string = input_string(1:max_length)
        end if
    end subroutine get_string
    
    function get_integer(prompt, default_value) result(value)
        character(len=*), intent(in) :: prompt
        integer, intent(in) :: default_value
        integer :: value
        character(len=20) :: input_str
        integer :: iostat
        
        write(*, '(A)', advance='no') trim(prompt)
        read(*, '(A)') input_str
        
        if (len_trim(input_str) == 0) then
            value = default_value
        else
            read(input_str, *, iostat=iostat) value
            if (iostat /= 0) value = default_value
        end if
    end function get_integer
    
    function get_confirmation(prompt) result(confirmed)
        character(len=*), intent(in) :: prompt
        logical :: confirmed
        character :: response
        
        write(*, '(A)', advance='no') trim(prompt)//' (y/N): '
        read(*, '(A)') response
        
        confirmed = (response == 'y' .or. response == 'Y')
    end function get_confirmation
    
    subroutine wait_for_key()
        character :: dummy
        integer :: iostat
        
        ! Use the same key reading mechanism as get_key for consistency
        call execute_command_line('stty raw -echo min 1 time 0 2>/dev/null')
        read(*, '(A)', iostat=iostat, advance='no') dummy
        call execute_command_line('stty sane 2>/dev/null')
        
        ! Ignore any read errors - we just want to wait for any input
    end subroutine wait_for_key
    
end module user_input