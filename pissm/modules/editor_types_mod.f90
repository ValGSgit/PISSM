module editor_types_mod
    use constants_mod, only: MAX_PATH_LENGTH
    implicit none
    
    ! Editor modes
    integer, parameter :: EDITOR_MODE_NORMAL = 1
    integer, parameter :: EDITOR_MODE_INSERT = 2
    integer, parameter :: EDITOR_MODE_COMMAND = 3
    
    ! Maximum lines and line length for text buffer
    integer, parameter :: MAX_EDITOR_LINES = 10000
    integer, parameter :: MAX_LINE_LENGTH = 1024
    
    ! Text buffer type for holding file content
    type :: text_buffer
        character(len=MAX_LINE_LENGTH), allocatable :: lines(:)
        integer :: line_count
        integer :: cursor_line
        integer :: cursor_col
        integer :: top_line          ! First visible line (for scrolling)
        logical :: modified
        character(len=MAX_PATH_LENGTH) :: filepath
    end type text_buffer
    
    ! Undo/redo operation types
    integer, parameter :: UNDO_INSERT_CHAR = 1
    integer, parameter :: UNDO_DELETE_CHAR = 2
    integer, parameter :: UNDO_INSERT_LINE = 3
    integer, parameter :: UNDO_DELETE_LINE = 4
    
    type :: undo_operation
        integer :: operation_type
        integer :: line_num
        integer :: col_num
        character(len=MAX_LINE_LENGTH) :: text
        character(len=MAX_LINE_LENGTH) :: old_text
    end type undo_operation
    
    ! Undo stack
    integer, parameter :: MAX_UNDO_OPERATIONS = 1000
    
    type :: undo_stack
        type(undo_operation) :: operations(MAX_UNDO_OPERATIONS)
        integer :: top
        integer :: current_pos
    end type undo_stack
    
    ! Editor state
    type :: editor_state
        type(text_buffer) :: buffer
        type(undo_stack) :: undo_history
        integer :: mode                      ! Current editor mode
        integer :: scroll_offset_line        ! Vertical scrolling offset
        integer :: scroll_offset_col         ! Horizontal scrolling offset
        logical :: show_line_numbers
        logical :: show_status_bar
        character(len=256) :: command_line   ! For command mode input
        character(len=256) :: search_pattern ! Last search pattern
        character(len=256) :: status_message ! Status/error messages
        integer :: message_timeout           ! Message display timeout
        logical :: quit_requested           ! Flag to return to file manager
        logical :: file_saved              ! Track if current file is saved
    end type editor_state
    
    ! Search result type
    type :: search_result
        integer :: line_num
        integer :: col_start
        integer :: col_end
        character(len=100) :: context
    end type search_result
    
    ! Maximum search results to display
    integer, parameter :: MAX_SEARCH_RESULTS = 100
    
contains
    
    ! Initialize a new text buffer
    subroutine init_text_buffer(buffer, filepath)
        type(text_buffer), intent(out) :: buffer
        character(len=*), intent(in), optional :: filepath
        
        ! Allocate initial space for lines
        allocate(buffer%lines(1000))
        buffer%line_count = 0
        buffer%cursor_line = 1
        buffer%cursor_col = 1
        buffer%top_line = 1
        buffer%modified = .false.
        
        if (present(filepath)) then
            buffer%filepath = filepath
        else
            buffer%filepath = ''
        end if
    end subroutine init_text_buffer
    
    ! Clean up text buffer
    subroutine cleanup_text_buffer(buffer)
        type(text_buffer), intent(inout) :: buffer
        
        if (allocated(buffer%lines)) then
            deallocate(buffer%lines)
        end if
        buffer%line_count = 0
        buffer%cursor_line = 1
        buffer%cursor_col = 1
        buffer%modified = .false.
        buffer%filepath = ''
    end subroutine cleanup_text_buffer
    
    ! Initialize editor state
    subroutine init_editor_state(editor)
        type(editor_state), intent(out) :: editor
        
        call init_text_buffer(editor%buffer)
        
        ! Initialize undo stack
        editor%undo_history%top = 0
        editor%undo_history%current_pos = 0
        
        ! Set default editor settings
        editor%mode = EDITOR_MODE_NORMAL
        editor%scroll_offset_line = 0
        editor%scroll_offset_col = 0
        editor%show_line_numbers = .true.
        editor%show_status_bar = .true.
        editor%command_line = ''
        editor%search_pattern = ''
        editor%status_message = ''
        editor%message_timeout = 0
        editor%quit_requested = .false.
        editor%file_saved = .true.
    end subroutine init_editor_state
    
    ! Clean up editor state
    subroutine cleanup_editor_state(editor)
        type(editor_state), intent(inout) :: editor
        
        call cleanup_text_buffer(editor%buffer)
    end subroutine cleanup_editor_state
    
    ! Get the current line text
    function get_current_line(buffer) result(line_text)
        type(text_buffer), intent(in) :: buffer
        character(len=MAX_LINE_LENGTH) :: line_text
        
        if (buffer%cursor_line >= 1 .and. buffer%cursor_line <= buffer%line_count) then
            line_text = buffer%lines(buffer%cursor_line)
        else
            line_text = ''
        end if
    end function get_current_line
    
    ! Check if cursor position is valid
    logical function is_cursor_valid(buffer)
        type(text_buffer), intent(in) :: buffer
        
        is_cursor_valid = (buffer%cursor_line >= 1 .and. buffer%cursor_line <= max(1, buffer%line_count) .and. &
                          buffer%cursor_col >= 1)
    end function is_cursor_valid
    
    ! Get line length (excluding trailing spaces)
    integer function get_line_length(buffer, line_num)
        type(text_buffer), intent(in) :: buffer
        integer, intent(in) :: line_num
        
        if (line_num >= 1 .and. line_num <= buffer%line_count) then
            get_line_length = len_trim(buffer%lines(line_num))
        else
            get_line_length = 0
        end if
    end function get_line_length

end module editor_types_mod