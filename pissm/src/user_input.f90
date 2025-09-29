module user_input
    use constants_mod
    implicit none
    
contains
    
    function get_key() result(key)
        character :: key
        integer :: iostat
        
        ! Read single character without waiting for Enter
        read(*, '(A)', iostat=iostat, advance='no') key
        if (iostat /= 0) key = ' '
    end function get_key
    
    subroutine get_string(prompt, input_string, max_length)
        character(len=*), intent(in) :: prompt
        character(len=*), intent(out) :: input_string
        integer, intent(in) :: max_length
        
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
        write(*, '(A)', advance='no') 'Press any key to continue...'
        read(*, '(A)') dummy
    end subroutine wait_for_key
    
end module user_input