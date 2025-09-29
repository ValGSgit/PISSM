module terminal_utils
    implicit none

contains

    ! Get terminal size using tput command (more reliable)
    subroutine get_terminal_size(width, height)
        use constants_mod, only: SCREEN_WIDTH, SCREEN_HEIGHT
        implicit none
        integer, intent(out) :: width, height
        integer :: status
        character(len=32) :: temp_file = '/tmp/pissm_size.tmp'
        character(len=10) :: size_str
        integer :: ios, unit_num

        ! Default fallback values
        width = 80
        height = 24

        ! Get terminal width using tput cols
        call execute_command_line('tput cols > '//trim(temp_file)//' 2>/dev/null', exitstat=status)
        if (status == 0) then
            open(newunit=unit_num, file=trim(temp_file), status='old', iostat=ios)
            if (ios == 0) then
                read(unit_num, '(A)', iostat=ios) size_str
                if (ios == 0) then
                    read(size_str, *, iostat=ios) width
                    if (ios /= 0 .or. width < 40) width = 80  ! Minimum width
                end if
                close(unit_num)
            end if
        end if

        ! Get terminal height using tput lines
        call execute_command_line('tput lines > '//trim(temp_file)//' 2>/dev/null', exitstat=status)
        if (status == 0) then
            open(newunit=unit_num, file=trim(temp_file), status='old', iostat=ios)
            if (ios == 0) then
                read(unit_num, '(A)', iostat=ios) size_str
                if (ios == 0) then
                    read(size_str, *, iostat=ios) height
                    if (ios /= 0 .or. height < 10) height = 24  ! Minimum height
                end if
                close(unit_num)
            end if
        end if

        ! Clean up temp file
        call execute_command_line('rm -f '//trim(temp_file)//' 2>/dev/null')

        ! Update global screen dimensions
        SCREEN_WIDTH = width
        SCREEN_HEIGHT = height

    end subroutine get_terminal_size

    ! Check if terminal has been resized (simplified - only check on manual refresh)
    logical function terminal_resized()
        implicit none
        
        ! For now, disable automatic resize detection to avoid performance issues
        ! User can manually refresh with 'r' key if terminal is resized
        terminal_resized = .false.
        
    end function terminal_resized

    ! Clear the entire screen and reset cursor
    subroutine clear_screen_adaptive()
        implicit none
        
        ! Clear screen using ANSI escape codes
        write(*, '(A)', advance='no') char(27)//'[2J'      ! Clear entire screen
        write(*, '(A)', advance='no') char(27)//'[1;1H'    ! Move to top-left
    end subroutine clear_screen_adaptive

end module terminal_utils