@echo off
REM PISSM Docker Helper Script for Windows
REM Makes it easy to build and run PISSM in Docker

setlocal enabledelayedexpansion

set IMAGE_NAME=pissm:latest
set COMMAND=%1
if "%COMMAND%"=="" set COMMAND=help

goto !COMMAND!

:help
echo PISSM Docker Helper for Windows
echo.
echo Usage: pissm.docker.bat [COMMAND] [OPTIONS]
echo.
echo Commands:
echo   build              Build Docker image
echo   run [PATH]         Run PISSM in container (default: C:\)
echo   shell              Launch interactive shell in container
echo   clean              Remove PISSM container and images
echo   help               Show this help message
echo.
echo Examples:
echo   pissm.docker.bat build                  # Build the image
echo   pissm.docker.bat run C:\Users\YourName  # Browse your user directory
echo   pissm.docker.bat run                    # Browse C:\
echo   pissm.docker.bat shell                  # Get PowerShell in container
echo   pissm.docker.bat clean                  # Clean up containers/images
echo.
goto end

:build
echo [INFO] Building Docker image...
if not exist Dockerfile (
    echo [ERROR] Dockerfile not found. Make sure you're in the PISSM root directory.
    goto end
)
docker build -t %IMAGE_NAME% .
if %ERRORLEVEL% EQU 0 (
    echo [SUCCESS] Image built successfully: %IMAGE_NAME%
    echo.
    echo Next steps:
    echo   pissm.docker.bat run          # Browse C:\
    echo   pissm.docker.bat shell        # Get a shell
) else (
    echo [ERROR] Build failed
)
goto end

:run
set PATH_TO_BROWSE=%2
if "!PATH_TO_BROWSE!"=="" set PATH_TO_BROWSE=C:\
echo [INFO] Checking if image exists...
docker image inspect %IMAGE_NAME% >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo [WARNING] Image %IMAGE_NAME% not found. Building...
    call :build
)
echo.
echo [INFO] Launching PISSM to browse: !PATH_TO_BROWSE!
echo.
echo Navigation Tips:
echo   j/k or arrows     - Move up/down
echo   gg/G              - Jump to start/end
echo   Enter             - Open file/directory
echo   b or arrows       - Go back
echo   q                 - Quit
echo   h                 - Toggle hidden files
echo   /                 - Search
echo.
docker run -it --rm ^
    -e TERM=xterm-256color ^
    -v %CD%:/workspace ^
    %IMAGE_NAME% !PATH_TO_BROWSE!
goto end

:shell
echo [INFO] Checking if image exists...
docker image inspect %IMAGE_NAME% >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo [WARNING] Image %IMAGE_NAME% not found. Building...
    call :build
)
echo [INFO] Launching PowerShell in container...
docker run -it --rm ^
    -e TERM=xterm-256color ^
    -v %CD%:/workspace ^
    %IMAGE_NAME% pwsh
goto end

:clean
echo [INFO] Cleaning up PISSM Docker resources...
docker ps -a --filter "name=pissm-app" --quiet
if %ERRORLEVEL% EQU 0 (
    echo [INFO] Removing container: pissm-app
    docker rm -f pissm-app 2>nul
)
echo [INFO] Removing image: %IMAGE_NAME%
docker rmi -f %IMAGE_NAME% 2>nul
echo [SUCCESS] Cleanup complete
goto end

:end
endlocal
