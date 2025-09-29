module editor_display
    use constants_mod
    use editor_types_mod
    use editor_core
    use utilities
    implicit none

contains

    ! Display the editor interface
    subroutine display_editor(editor)
        type(editor_state), intent(inout) :: editor
        integer :: start_line, end_line, screen_row, i
        integer :: line_num_width
        character(len=10) :: line_num_str
        character(len=MAX_LINE_LENGTH) :: display_line
        
        call clear_screen()
        
        ! Calculate visible lines
        call get_visible_lines(editor%buffer, SCREEN_HEIGHT, start_line, end_line)
        editor%buffer%top_line = start_line
        
        ! Calculate line number width
        line_num_width = max(3, len(trim(adjustl(char(editor%buffer%line_count + 48)))))
        
        ! Display title bar
        call set_cursor_position(1, 1)
        if (len_trim(editor%buffer%filepath) > 0) then
            call print_colored('PISSM Editor - '//trim(editor%buffer%filepath), COLOR_BOLD//COLOR_CYAN)
        else
            call print_colored('PISSM Editor - [No Name]', COLOR_BOLD//COLOR_CYAN)
        end if
        
        if (editor%buffer%modified) then
            call print_colored(' [Modified]', COLOR_YELLOW//COLOR_BOLD)
        end if
        
        ! Display editor content
        screen_row = 3
        do i = start_line, end_line
            if (screen_row > SCREEN_HEIGHT - 2) exit
            
            call set_cursor_position(screen_row, 1)
            
            ! Display line number if enabled
            if (editor%show_line_numbers) then
                write(line_num_str, '(I0)') i
                if (i == editor%buffer%cursor_line) then
                    call print_colored(repeat(' ', line_num_width - len_trim(line_num_str)), COLOR_DIM)
                    call print_colored(trim(line_num_str), COLOR_BRIGHT_YELLOW//COLOR_BOLD)
                else
                    call print_colored(repeat(' ', line_num_width - len_trim(line_num_str)), COLOR_DIM)
                    call print_colored(trim(line_num_str), COLOR_DIM)
                end if
                call print_colored('│', COLOR_DIM)
                write(*, '(A)', advance='no') ' '
            end if
            
            ! Get line to display
            if (i <= editor%buffer%line_count) then
                display_line = editor%buffer%lines(i)
            else
                display_line = '~'  ! Vim-style empty line indicator
                call print_colored(display_line, COLOR_DIM)
                screen_row = screen_row + 1
                cycle
            end if
            
            ! Highlight current line
            if (i == editor%buffer%cursor_line) then
                ! Display line with cursor highlighting
                call display_line_with_cursor(display_line, editor%buffer%cursor_col)
            else
                ! Regular line display
                write(*, '(A)', advance='no') trim(display_line)
            end if
            
            screen_row = screen_row + 1
        end do
        
        ! Display status bar
        call display_status_bar(editor)
        
        ! Position cursor for visual feedback
        call position_cursor_on_screen(editor, line_num_width)
        
    end subroutine display_editor

    ! Display a line with cursor highlighting
    subroutine display_line_with_cursor(line, cursor_col)
        character(len=*), intent(in) :: line
        integer, intent(in) :: cursor_col
        integer :: line_len, i
        character :: current_char
        
        line_len = len_trim(line)
        
        ! Display characters up to cursor
        if (cursor_col > 1) then
            if (cursor_col - 1 <= line_len) then
                write(*, '(A)', advance='no') line(1:cursor_col-1)
            else
                write(*, '(A)', advance='no') trim(line)
                do i = line_len + 1, cursor_col - 1
                    write(*, '(A)', advance='no') ' '
                end do
            end if
        end if
        
        ! Highlight cursor position
        if (cursor_col <= line_len) then
            current_char = line(cursor_col:cursor_col)
            if (current_char == ' ') current_char = '_'  ! Make spaces visible
            call print_colored(current_char, COLOR_BLACK//COLOR_BRIGHT_WHITE)  ! Reverse video
        else
            call print_colored(' ', COLOR_BLACK//COLOR_BRIGHT_WHITE)  ! Cursor at end of line
        end if
        
        ! Display rest of line
        if (cursor_col < line_len) then
            write(*, '(A)', advance='no') line(cursor_col+1:line_len)
        end if
        
    end subroutine display_line_with_cursor

    ! Display status bar at bottom
    subroutine display_status_bar(editor)
        type(editor_state), intent(in) :: editor
        character(len=20) :: mode_str, pos_str
        integer :: line_percent
        
        call set_cursor_position(SCREEN_HEIGHT - 1, 1)
        call print_colored(repeat('─', SCREEN_WIDTH), COLOR_DIM)
        
        call set_cursor_position(SCREEN_HEIGHT, 1)
        
        ! Mode indicator
        select case (editor%mode)
            case (EDITOR_MODE_NORMAL)
                mode_str = '-- NORMAL --'
                call print_colored(mode_str, COLOR_GREEN//COLOR_BOLD)
            case (EDITOR_MODE_INSERT)
                mode_str = '-- INSERT --'
                call print_colored(mode_str, COLOR_BLUE//COLOR_BOLD)
            case (EDITOR_MODE_COMMAND)
                mode_str = '-- COMMAND --'
                call print_colored(mode_str, COLOR_MAGENTA//COLOR_BOLD)
        end select
        
        ! File info
        write(*, '(A)', advance='no') ' │ '
        if (editor%buffer%modified) then
            call print_colored('[+]', COLOR_YELLOW)
        else
            write(*, '(A)', advance='no') '   '
        end if
        
        ! Position info
        if (editor%buffer%line_count > 0) then
            line_percent = (editor%buffer%cursor_line * 100) / editor%buffer%line_count
        else
            line_percent = 100
        end if
        
        write(pos_str, '(I0,A,I0,A,I0,A)') editor%buffer%cursor_line, ',', editor%buffer%cursor_col, &
                                          ' (', line_percent, '%)'
        
        write(*, '(A)', advance='no') ' │ ' // trim(pos_str)
        
        ! Display status message if any
        if (len_trim(editor%status_message) > 0) then
            write(*, '(A)', advance='no') ' │ '
            call print_colored(trim(editor%status_message), COLOR_YELLOW)
        end if
        
    end subroutine display_status_bar

    ! Position terminal cursor at the editor cursor location
    subroutine position_cursor_on_screen(editor, line_num_width)
        type(editor_state), intent(in) :: editor
        integer, intent(in) :: line_num_width
        integer :: screen_row, screen_col
        integer :: visible_line_offset
        
        ! Calculate screen row for cursor
        visible_line_offset = editor%buffer%cursor_line - editor%buffer%top_line
        screen_row = 3 + visible_line_offset
        
        ! Calculate screen column
        screen_col = editor%buffer%cursor_col
        if (editor%show_line_numbers) then
            screen_col = screen_col + line_num_width + 3  ! Line number + "│ "
        end if
        
        ! Only position cursor if it's visible
        if (screen_row >= 3 .and. screen_row <= SCREEN_HEIGHT - 2) then
            call set_cursor_position(screen_row, screen_col)
        end if
        
    end subroutine position_cursor_on_screen

    ! Display help screen for editor
    subroutine display_editor_help()
        call clear_screen()
        call print_colored('PISSM Text Editor - Help', COLOR_BOLD//COLOR_CYAN)
        write(*,*)
        write(*,*)
        
        call print_colored('NORMAL MODE:', COLOR_GREEN//COLOR_BOLD)
        write(*,*)
        write(*,'(A)') '  Movement:'
        write(*,'(A)') '    h, j, k, l  - Move cursor left, down, up, right'
        write(*,'(A)') '    w, b        - Move word forward, backward'
        write(*,'(A)') '    0, $        - Move to beginning/end of line'
        write(*,'(A)') '    gg, G       - Move to first/last line'
        write(*,*)
        write(*,'(A)') '  Editing:'
        write(*,'(A)') '    i, a        - Enter insert mode (before/after cursor)'
        write(*,'(A)') '    o, O        - Open new line (below/above)'
        write(*,'(A)') '    x, X        - Delete character (at/before cursor)'
        write(*,'(A)') '    dd          - Delete line'
        write(*,'(A)') '    u           - Undo'
        write(*,'(A)') '    Ctrl+r      - Redo'
        write(*,*)
        write(*,'(A)') '  Search:'
        write(*,'(A)') '    /           - Search forward'
        write(*,'(A)') '    ?           - Search backward'
        write(*,'(A)') '    n, N        - Next/previous search result'
        write(*,*)
        
        call print_colored('INSERT MODE:', COLOR_BLUE//COLOR_BOLD)
        write(*,*)
        write(*,'(A)') '    ESC         - Return to normal mode'
        write(*,'(A)') '    Printable characters are inserted at cursor'
        write(*,'(A)') '    Enter       - Insert new line'
        write(*,'(A)') '    Backspace   - Delete character before cursor'
        write(*,*)
        
        call print_colored('COMMAND MODE:', COLOR_MAGENTA//COLOR_BOLD)
        write(*,*)
        write(*,'(A)') '    :w          - Save file'
        write(*,'(A)') '    :q          - Quit to file manager'
        write(*,'(A)') '    :wq         - Save and quit'
        write(*,'(A)') '    :q!         - Quit without saving'
        write(*,'(A)') '    :help       - Show this help'
        write(*,*)
        
        call print_colored('GLOBAL:', COLOR_YELLOW//COLOR_BOLD)
        write(*,*)
        write(*,'(A)') '    ESC         - Return to file manager'
        write(*,'(A)') '    ?           - Show this help'
        write(*,*)
        
        call set_cursor_position(SCREEN_HEIGHT, 1)
        call print_colored('Press any key to continue...', COLOR_DIM)
        
    end subroutine display_editor_help

    ! Display a simple message at the bottom
    subroutine show_editor_message(editor, message, color)
        type(editor_state), intent(inout) :: editor
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: color
        character(len=*), parameter :: default_color = COLOR_YELLOW
        
        editor%status_message = message
        editor%message_timeout = 50  ! Display for ~50 refresh cycles
        
        call set_cursor_position(SCREEN_HEIGHT, 1)
        call clear_line()
        
        if (present(color)) then
            call print_colored(trim(message), color)
        else
            call print_colored(trim(message), default_color)
        end if
        
    end subroutine show_editor_message

    ! Clear status message if timeout reached
    subroutine update_editor_message(editor)
        type(editor_state), intent(inout) :: editor
        
        if (editor%message_timeout > 0) then
            editor%message_timeout = editor%message_timeout - 1
            if (editor%message_timeout == 0) then
                editor%status_message = ''
            end if
        end if
        
    end subroutine update_editor_message

end module editor_display