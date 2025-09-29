module constants_mod
    implicit none
    
    ! Screen dimensions and colors
    integer, parameter :: MAX_PATH_LENGTH = 512
    integer, parameter :: MAX_FILES = 1000
    integer, parameter :: MAX_FILENAME = 256
    integer, parameter :: SCREEN_WIDTH = 140
    integer, parameter :: SCREEN_HEIGHT = 40
    
    ! ANSI color codes
    character(len=*), parameter :: COLOR_RESET = char(27)//'[0m'
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
    
    ! Extended color codes for better visual appeal
    character(len=*), parameter :: COLOR_BRIGHT_GREEN = char(27)//'[92m'
    character(len=*), parameter :: COLOR_BRIGHT_BLUE = char(27)//'[94m'
    character(len=*), parameter :: COLOR_BRIGHT_CYAN = char(27)//'[96m'
    character(len=*), parameter :: COLOR_BRIGHT_YELLOW = char(27)//'[93m'
    character(len=*), parameter :: COLOR_BRIGHT_MAGENTA = char(27)//'[95m'
    
    ! File type indicators
    character(len=*), parameter :: ICON_DIRECTORY = '📁'
    character(len=*), parameter :: ICON_FILE = '📄'
    character(len=*), parameter :: ICON_EXECUTABLE = '⚡'
    character(len=*), parameter :: ICON_LINK = '🔗'
    character(len=*), parameter :: ICON_ARCHIVE = '📦'
    character(len=*), parameter :: ICON_IMAGE = '🖼️ '
    character(len=*), parameter :: ICON_TEXT = '📝'
    character(len=*), parameter :: ICON_CODE = '💾'
    
    ! Menu options
    integer, parameter :: MENU_BROWSE = 1
    integer, parameter :: MENU_MOVE = 2
    integer, parameter :: MENU_COPY = 3
    integer, parameter :: MENU_DELETE = 4
    integer, parameter :: MENU_CREATE = 5
    integer, parameter :: MENU_SEARCH = 6
    integer, parameter :: MENU_EXIT = 7
    
end module constants_mod