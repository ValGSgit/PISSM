!> \brief Constants Module
!! 
!! This module defines all global constants used throughout PISSM.
!! It includes:
!!  - Memory and string size limits
!!  - ANSI color codes for terminal output
!!  - File type icons/indicators  
!!  - Menu option identifiers
!!
!! \author Your Name
!! \date 2024

module constants_mod
    implicit none
    
    !> Configuration Constants
    !! Memory and string length limits
    integer, parameter :: MAX_PATH_LENGTH = 256  !! Maximum path string length
    integer, parameter :: MAX_FILES = 50         !! Maximum files per directory
    integer, parameter :: MAX_FILENAME = 64      !! Maximum filename length
    
    !> Dynamic Screen Dimensions
    !! These are set at runtime based on terminal size
    integer :: SCREEN_WIDTH = 80   !! Current terminal width
    integer :: SCREEN_HEIGHT = 24  !! Current terminal height
    
    ! ========== ANSI Color Codes ==========
    
    character(len=*), parameter :: COLOR_RESET = char(27)//'[0m'
    character(len=*), parameter :: COLOR_BLACK = char(27)//'[30m'
    character(len=*), parameter :: COLOR_RED = char(27)//'[31m'
    character(len=*), parameter :: COLOR_GREEN = char(27)//'[32m'
    character(len=*), parameter :: COLOR_YELLOW = char(27)//'[33m'
    character(len=*), parameter :: COLOR_BLUE = char(27)//'[34m'
    character(len=*), parameter :: COLOR_MAGENTA = char(27)//'[35m'
    character(len=*), parameter :: COLOR_CYAN = char(27)//'[36m'
    character(len=*), parameter :: COLOR_WHITE = char(27)//'[37m'
    character(len=*), parameter :: COLOR_BOLD = char(27)//'[1m'
    character(len=*), parameter :: COLOR_DIM = char(27)//'[2m'
    character(len=*), parameter :: COLOR_UNDERLINE = char(27)//'[4m'
    
    ! ========== Bright/Extended Colors ==========
    
    character(len=*), parameter :: COLOR_BRIGHT_RED = char(27)//'[91m'
    character(len=*), parameter :: COLOR_BRIGHT_GREEN = char(27)//'[92m'
    character(len=*), parameter :: COLOR_BRIGHT_BLUE = char(27)//'[94m'
    character(len=*), parameter :: COLOR_BRIGHT_CYAN = char(27)//'[96m'
    character(len=*), parameter :: COLOR_BRIGHT_YELLOW = char(27)//'[93m'
    character(len=*), parameter :: COLOR_BRIGHT_MAGENTA = char(27)//'[95m'
    character(len=*), parameter :: COLOR_BRIGHT_WHITE = char(27)//'[97m'
    
    ! ========== File Type Icons ==========
    
    character(len=*), parameter :: ICON_DIRECTORY = '📁'
    character(len=*), parameter :: ICON_FILE = '📄'
    character(len=*), parameter :: ICON_EXECUTABLE = '⚡'
    character(len=*), parameter :: ICON_LINK = '🔗'
    character(len=*), parameter :: ICON_ARCHIVE = '📦'
    character(len=*), parameter :: ICON_IMAGE = '🖼️ '
    character(len=*), parameter :: ICON_TEXT = '📝'
    character(len=*), parameter :: ICON_CODE = '💾'
    
    ! ========== Menu Options ==========
    
    integer, parameter :: MENU_BROWSE = 1
    integer, parameter :: MENU_MOVE = 2
    integer, parameter :: MENU_COPY = 3
    integer, parameter :: MENU_DELETE = 4
    integer, parameter :: MENU_CREATE = 5
    integer, parameter :: MENU_SEARCH = 6
    integer, parameter :: MENU_EXIT = 7
    
end module constants_mod