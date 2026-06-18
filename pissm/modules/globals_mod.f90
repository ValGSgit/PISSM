!> \brief Global Variables Module
!!
!! Maintains global state variables shared across the application.
!! These include the UI state, directory tree structure, and program control flags.
!!
!! \author Your Name
!! \date 2024

module globals_mod
    use types_mod
    implicit none
    
    !> Current Interface State
    !! Tracks user selection, view mode, paths, and clipboard state
    type(interface_state) :: ui_state
    
    !> Root Directory Node
    !! Pointer to the root of the directory tree
    type(directory_node), pointer :: root_node => null()
    
    !> Current Directory Node
    !! Pointer to the currently displayed directory
    type(directory_node), pointer :: current_node => null()
    
    !> Program Running Flag
    !! Controls main event loop; set to false to exit cleanly
    logical :: program_running = .true.
    
    !> Original Working Directory
    !! Stores the initial directory for reference
    character(len=MAX_PATH_LENGTH) :: original_path

end module globals_mod