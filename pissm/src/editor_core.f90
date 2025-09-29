module editor_core
    use editor_types_mod
    use constants_mod
    implicit none

contains

    ! Load file into text buffer
    subroutine load_file_to_buffer(buffer, filepath, success)
        type(text_buffer), intent(inout) :: buffer
        character(len=*), intent(in) :: filepath
        logical, intent(out) :: success
        integer :: unit_num, ios, line_num
        character(len=MAX_LINE_LENGTH) :: line
        
        success = .false.
        buffer%filepath = filepath
        buffer%line_count = 0
        buffer%cursor_line = 1
        buffer%cursor_col = 1
        buffer%top_line = 1
        buffer%modified = .false.
        
        ! Try to open the file
        open(newunit=unit_num, file=trim(filepath), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            ! File doesn't exist or can't be opened - create empty buffer
            if (allocated(buffer%lines)) deallocate(buffer%lines)
            allocate(buffer%lines(100))
            buffer%lines(1) = ''
            buffer%line_count = 1
            buffer%modified = .true.  ! New file needs saving
            success = .true.
            return
        end if
        
        ! Count lines first to allocate proper space
        line_num = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1
        end do
        
        ! Rewind and allocate space
        rewind(unit_num)
        if (allocated(buffer%lines)) deallocate(buffer%lines)
        allocate(buffer%lines(max(line_num + 100, 1000)))  ! Extra space for editing
        
        ! Read lines into buffer
        line_num = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1
            if (line_num <= size(buffer%lines)) then
                buffer%lines(line_num) = line
            end if
        end do
        
        close(unit_num)
        buffer%line_count = max(line_num, 1)
        
        ! If file was empty, add one empty line
        if (buffer%line_count == 0) then
            buffer%line_count = 1
            buffer%lines(1) = ''
        end if
        
        success = .true.
    end subroutine load_file_to_buffer

    ! Save buffer to file
    subroutine save_buffer_to_file(buffer, success, error_msg)
        type(text_buffer), intent(inout) :: buffer
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        integer :: unit_num, ios, i
        
        success = .false.
        error_msg = ''
        
        if (len_trim(buffer%filepath) == 0) then
            error_msg = 'No filename specified'
            return
        end if
        
        ! Open file for writing
        open(newunit=unit_num, file=trim(buffer%filepath), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            error_msg = 'Cannot open file for writing'
            return
        end if
        
        ! Write all lines
        do i = 1, buffer%line_count
            write(unit_num, '(A)', iostat=ios) trim(buffer%lines(i))
            if (ios /= 0) then
                error_msg = 'Error writing to file'
                close(unit_num)
                return
            end if
        end do
        
        close(unit_num)
        buffer%modified = .false.
        success = .true.
    end subroutine save_buffer_to_file

    ! Insert character at cursor position
    subroutine insert_character(buffer, char)
        type(text_buffer), intent(inout) :: buffer
        character, intent(in) :: char
        character(len=MAX_LINE_LENGTH) :: current_line, new_line
        integer :: line_len
        
        if (buffer%cursor_line < 1 .or. buffer%cursor_line > buffer%line_count) return
        
        current_line = buffer%lines(buffer%cursor_line)
        line_len = len_trim(current_line)
        
        ! Ensure cursor column is valid
        if (buffer%cursor_col < 1) buffer%cursor_col = 1
        if (buffer%cursor_col > line_len + 1) buffer%cursor_col = line_len + 1
        
        ! Insert character
        if (buffer%cursor_col == 1) then
            new_line = char // current_line(1:MAX_LINE_LENGTH-1)
        else if (buffer%cursor_col > line_len) then
            new_line = current_line(1:min(line_len, MAX_LINE_LENGTH-1)) // char
        else
            new_line = current_line(1:buffer%cursor_col-1) // char // &
                      current_line(buffer%cursor_col:MAX_LINE_LENGTH-buffer%cursor_col)
        end if
        
        buffer%lines(buffer%cursor_line) = new_line
        buffer%cursor_col = buffer%cursor_col + 1
        buffer%modified = .true.
    end subroutine insert_character

    ! Delete character at cursor position
    subroutine delete_character(buffer)
        type(text_buffer), intent(inout) :: buffer
        character(len=MAX_LINE_LENGTH) :: current_line, new_line
        integer :: line_len
        
        if (buffer%cursor_line < 1 .or. buffer%cursor_line > buffer%line_count) return
        
        current_line = buffer%lines(buffer%cursor_line)
        line_len = len_trim(current_line)
        
        if (buffer%cursor_col < 1 .or. buffer%cursor_col > line_len) return
        
        ! Delete character
        if (buffer%cursor_col == 1 .and. line_len == 1) then
            new_line = ''
        else if (buffer%cursor_col == 1) then
            new_line = current_line(2:)
        else if (buffer%cursor_col > line_len) then
            new_line = current_line
        else
            new_line = current_line(1:buffer%cursor_col-1) // current_line(buffer%cursor_col+1:)
        end if
        
        buffer%lines(buffer%cursor_line) = new_line
        buffer%modified = .true.
    end subroutine delete_character

    ! Insert new line at cursor position
    subroutine insert_new_line(buffer)
        type(text_buffer), intent(inout) :: buffer
        character(len=MAX_LINE_LENGTH) :: current_line, new_line
        integer :: i, line_len
        
        if (buffer%cursor_line < 1 .or. buffer%cursor_line > buffer%line_count) return
        if (buffer%line_count >= size(buffer%lines)) return  ! Buffer full
        
        current_line = buffer%lines(buffer%cursor_line)
        line_len = len_trim(current_line)
        
        ! Split current line at cursor position
        if (buffer%cursor_col <= 1) then
            new_line = current_line
            buffer%lines(buffer%cursor_line) = ''
        else if (buffer%cursor_col > line_len) then
            new_line = ''
        else
            new_line = current_line(buffer%cursor_col:)
            buffer%lines(buffer%cursor_line) = current_line(1:buffer%cursor_col-1)
        end if
        
        ! Shift all lines down
        do i = buffer%line_count, buffer%cursor_line + 1, -1
            buffer%lines(i + 1) = buffer%lines(i)
        end do
        
        ! Insert new line
        buffer%cursor_line = buffer%cursor_line + 1
        buffer%lines(buffer%cursor_line) = new_line
        buffer%line_count = buffer%line_count + 1
        buffer%cursor_col = 1
        buffer%modified = .true.
    end subroutine insert_new_line

    ! Delete current line
    subroutine delete_line(buffer)
        type(text_buffer), intent(inout) :: buffer
        integer :: i
        
        if (buffer%cursor_line < 1 .or. buffer%cursor_line > buffer%line_count) return
        if (buffer%line_count == 1) then
            buffer%lines(1) = ''
            buffer%cursor_col = 1
            buffer%modified = .true.
            return
        end if
        
        ! Shift lines up
        do i = buffer%cursor_line, buffer%line_count - 1
            buffer%lines(i) = buffer%lines(i + 1)
        end do
        
        buffer%line_count = buffer%line_count - 1
        
        ! Adjust cursor position
        if (buffer%cursor_line > buffer%line_count) then
            buffer%cursor_line = buffer%line_count
        end if
        buffer%cursor_col = 1
        buffer%modified = .true.
    end subroutine delete_line

    ! Move cursor up
    subroutine move_cursor_up(buffer)
        type(text_buffer), intent(inout) :: buffer
        integer :: target_line_len
        
        if (buffer%cursor_line > 1) then
            buffer%cursor_line = buffer%cursor_line - 1
            target_line_len = len_trim(buffer%lines(buffer%cursor_line))
            if (buffer%cursor_col > target_line_len + 1) then
                buffer%cursor_col = target_line_len + 1
            end if
        end if
    end subroutine move_cursor_up

    ! Move cursor down
    subroutine move_cursor_down(buffer)
        type(text_buffer), intent(inout) :: buffer
        integer :: target_line_len
        
        if (buffer%cursor_line < buffer%line_count) then
            buffer%cursor_line = buffer%cursor_line + 1
            target_line_len = len_trim(buffer%lines(buffer%cursor_line))
            if (buffer%cursor_col > target_line_len + 1) then
                buffer%cursor_col = target_line_len + 1
            end if
        end if
    end subroutine move_cursor_down

    ! Move cursor left
    subroutine move_cursor_left(buffer)
        type(text_buffer), intent(inout) :: buffer
        
        if (buffer%cursor_col > 1) then
            buffer%cursor_col = buffer%cursor_col - 1
        else if (buffer%cursor_line > 1) then
            ! Move to end of previous line
            buffer%cursor_line = buffer%cursor_line - 1
            buffer%cursor_col = len_trim(buffer%lines(buffer%cursor_line)) + 1
        end if
    end subroutine move_cursor_left

    ! Move cursor right
    subroutine move_cursor_right(buffer)
        type(text_buffer), intent(inout) :: buffer
        integer :: line_len
        
        line_len = len_trim(buffer%lines(buffer%cursor_line))
        
        if (buffer%cursor_col <= line_len) then
            buffer%cursor_col = buffer%cursor_col + 1
        else if (buffer%cursor_line < buffer%line_count) then
            ! Move to beginning of next line
            buffer%cursor_line = buffer%cursor_line + 1
            buffer%cursor_col = 1
        end if
    end subroutine move_cursor_right

    ! Move cursor to beginning of line
    subroutine move_cursor_home(buffer)
        type(text_buffer), intent(inout) :: buffer
        
        buffer%cursor_col = 1
    end subroutine move_cursor_home

    ! Move cursor to end of line
    subroutine move_cursor_end(buffer)
        type(text_buffer), intent(inout) :: buffer
        
        buffer%cursor_col = len_trim(buffer%lines(buffer%cursor_line)) + 1
    end subroutine move_cursor_end

    ! Get visible line range for display
    subroutine get_visible_lines(buffer, screen_height, start_line, end_line)
        type(text_buffer), intent(in) :: buffer
        integer, intent(in) :: screen_height
        integer, intent(out) :: start_line, end_line
        integer :: visible_lines
        
        visible_lines = screen_height - 4  ! Leave space for header and status
        
        ! Ensure cursor is visible
        if (buffer%cursor_line < buffer%top_line) then
            start_line = buffer%cursor_line
        else if (buffer%cursor_line >= buffer%top_line + visible_lines) then
            start_line = buffer%cursor_line - visible_lines + 1
        else
            start_line = buffer%top_line
        end if
        
        start_line = max(1, start_line)
        end_line = min(buffer%line_count, start_line + visible_lines - 1)
    end subroutine get_visible_lines

end module editor_core