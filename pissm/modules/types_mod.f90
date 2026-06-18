!> \brief Type Definitions Module
!!
!! Defines all custom data types used throughout PISSM:
!!  - file_entry: Represents a single file/directory with metadata
!!  - directory_node: Tree node for directory hierarchy
!!  - interface_state: Current UI state and user selection
!!
!! \author Your Name
!! \date 2024

module types_mod
    use constants_mod
    implicit none
    
    !> File Entry Type
    !! Stores metadata for a single file or directory from ls output
    type :: file_entry
        character(len=MAX_FILENAME) :: name              !! Filename (with path)
        character(len=MAX_PATH_LENGTH) :: full_path      !! Complete file path
        logical :: is_directory                          !! Directory flag
        integer(kind=8) :: size                          !! File size (bytes)
        character(len=12) :: modified_time               !! Last modified time
        character(len=10) :: permissions                 !! Unix permissions string
        character(len=16) :: owner_group                 !! Owner.group string
        integer :: link_count                            !! Hard link count
        logical :: selected                              !! User selection flag
    end type file_entry
    
    !> Directory Node Type
    !! Represents a directory in a tree structure for efficient navigation
    type :: directory_node
        character(len=MAX_PATH_LENGTH) :: path           !! Directory path
        type(file_entry), allocatable :: files(:)        !! Array of files
        integer :: file_count                            !! Number of files
        logical :: expanded                              !! Expansion state
        type(directory_node), pointer :: parent => null() !! Parent directory node
        type(directory_node), pointer :: children(:) => null() !! Child directories
        integer :: child_count                           !! Number of children
    end type directory_node
    
    !> Interface State Type
    !! Maintains current UI state and user interaction context
    type :: interface_state
        integer :: current_selection                     !! Current highlighted item
        integer :: scroll_position                       !! Current scroll position
        integer :: view_mode                             !! Display mode
        logical :: show_hidden                           !! Show hidden files flag
        character(len=MAX_PATH_LENGTH) :: current_path   !! Current working directory
        character(len=MAX_PATH_LENGTH) :: clipboard_path !! Clipboard item path
        integer :: clipboard_operation                   !! 1=copy, 2=move
        character(len=MAX_PATH_LENGTH) :: previous_path  !! Previous directory (for back navigation)
        integer :: previous_selection                    !! Previous cursor position
    end type interface_state
    
end module types_mod