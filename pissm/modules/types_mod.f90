module types_mod
    use constants_mod
    implicit none
    
    type :: file_entry
        character(len=MAX_FILENAME) :: name
        character(len=MAX_PATH_LENGTH) :: full_path
        logical :: is_directory
        integer(kind=8) :: size  ! Use 8-byte integer for large files
        character(len=12) :: modified_time  ! Reduced from 16
        character(len=10) :: permissions
        character(len=16) :: owner_group    ! Reduced from 24  
        integer :: link_count
        logical :: selected
    end type file_entry
    
    type :: directory_node
        character(len=MAX_PATH_LENGTH) :: path
        type(file_entry), allocatable :: files(:)
        integer :: file_count
        logical :: expanded
        type(directory_node), pointer :: parent => null()
        type(directory_node), pointer :: children(:) => null()
        integer :: child_count
    end type directory_node
    
    type :: interface_state
        integer :: current_selection
        integer :: scroll_position
        integer :: view_mode
        logical :: show_hidden
        character(len=MAX_PATH_LENGTH) :: current_path
        character(len=MAX_PATH_LENGTH) :: clipboard_path
        integer :: clipboard_operation  ! 1=copy, 2=move
        character(len=MAX_PATH_LENGTH) :: previous_path  ! For navigation memory
        integer :: previous_selection  ! Remember cursor position
    end type interface_state
    
end module types_mod