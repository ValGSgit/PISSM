module globals_mod
    use types_mod
    implicit none
    
    type(interface_state) :: ui_state
    type(directory_node), pointer :: root_node => null()
    type(directory_node), pointer :: current_node => null()
    
    logical :: program_running = .true.
    character(len=MAX_PATH_LENGTH) :: original_path

end module globals_mod