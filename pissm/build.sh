#!/bin/bash

# Build script for PISSM
# Personal Interface System Structure & Modifications

set -e

# Get script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default build type
BUILD_TYPE="${1:-release}"

# Compiler to use
FC="${FC:-gfortran}"

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to display usage
usage() {
    cat << EOF
Usage: ./build.sh [BUILD_TYPE]

Build types:
  release (default) - Optimized build, symbols stripped
  debug            - Build with debugging symbols and runtime checks
  clean            - Clean build artifacts
  help             - Show this message

Examples:
  ./build.sh              # Build release version
  ./build.sh debug        # Build debug version
  ./build.sh clean        # Clean artifacts

Environment variables:
  FC                     - Fortran compiler (default: gfortran)
EOF
    exit 0
}

# Handle help/usage
if [[ "$BUILD_TYPE" == "help" ]] || [[ "$BUILD_TYPE" == "-h" ]] || [[ "$BUILD_TYPE" == "--help" ]]; then
    usage
fi

# Check if gfortran is available
if ! command -v "$FC" &> /dev/null; then
    print_error "$FC compiler not found. Please install gfortran."
    echo "On Ubuntu/Debian: sudo apt install gfortran"
    echo "On RHEL/CentOS: sudo yum install gcc-gfortran"
    echo "On macOS: brew install gcc"
    exit 1
fi

print_status "Using Fortran compiler: $FC"
print_status "Build type: $BUILD_TYPE"

# Handle clean target
if [[ "$BUILD_TYPE" == "clean" ]]; then
    print_status "Cleaning build artifacts..."
    rm -rf obj bin
    print_success "Clean completed successfully!"
    exit 0
fi

# Validate build type
if [[ "$BUILD_TYPE" != "release" && "$BUILD_TYPE" != "debug" ]]; then
    print_error "Unknown build type: $BUILD_TYPE"
    echo "Valid options: release, debug, clean, help"
    exit 1
fi

# Create directory structure
print_status "Creating directory structure..."
mkdir -p obj bin

# Check if all source files exist
print_status "Checking source files..."
required_files=(
    "modules/constants_mod.f90"
    "modules/types_mod.f90"
    "modules/globals_mod.f90"
    "modules/editor_types_mod.f90"
    "modules/terminal_utils.f90"
    "src/utilities.f90"
    "src/file_operations.f90"
    "src/user_input.f90"
    "src/directory_tree.f90"
    "src/interface_manager.f90"
    "src/editor_display.f90"
    "src/editor_core.f90"
    "src/main.f90"
)

for file in "${required_files[@]}"; do
    if [ ! -f "$file" ]; then
        print_warning "Source file missing: $file (may not be critical)"
    fi
done

# Set compiler flags based on build type
if [[ "$BUILD_TYPE" == "debug" ]]; then
    FFLAGS="-std=f2008 -Wall -Wextra -pedantic -g -O0 -fcheck=all -fbacktrace"
    TARGET="bin/pissm-debug"
    print_status "Building DEBUG version (with debugging symbols and runtime checks)..."
else
    FFLAGS="-std=f2008 -Wall -Wextra -pedantic -O2 -s"
    TARGET="bin/pissm"
    print_status "Building RELEASE version (optimized, stripped)..."
fi

# Use make for actual compilation (better dependency handling)
print_status "Invoking make..."
if [[ "$BUILD_TYPE" == "debug" ]]; then
    make debug
else
    make release
fi

# Check if build was successful
if [ -f "bin/pissm" ] || [ -f "bin/pissm-debug" ]; then
    print_success "PISSM built successfully!"
    echo
    echo -e "${GREEN}🎉 PISSM - Personal Interface System Structure & Modifications ${GREEN}🎉${NC}"
    echo -e "${BLUE}=================================================================${NC}"
    echo -e "${BLUE}📁 Executable location:${NC} $TARGET"
    echo -e "${BLUE}🚀 Run with:${NC} ./$TARGET"
    echo
    echo -e "${YELLOW}✨ Enhanced Features:${NC}"
    echo -e "  📊 Detailed file information with permissions"
    echo -e "  🎨 Beautiful visual interface with colors"
    echo -e "  ⚡ Fast vim-like navigation (j/k, gg, G)"
    echo -e "  🔍 Powerful search (/, s for advanced search)"
    echo -e "  🗂️  Complete file operations (copy, move, delete)"
    echo -e "  👁️  Hidden file toggle (h key)"
    echo -e "  📋 File details view (i key)"
    echo -e "  🏠 Quick home directory access (~ key)"
    echo
    echo -e "${YELLOW}🎯 Quick Start:${NC}"
    echo -e "  • Use ${GREEN}j/k${NC} or arrow keys to navigate"
    echo -e "  • Press ${GREEN}Enter${NC} to open files/directories"
    echo -e "  • Press ${GREEN}?${NC} for complete help"
    echo -e "  • Press ${GREEN}q${NC} to quit"
else
    print_error "Build failed! Check compilation errors above."
    exit 1
fi

# Make executable (should already be, but ensure it)
chmod +x "$TARGET"

print_success "Build completed successfully!"
echo
echo "To run PISSM:"
echo "  $TARGET"
echo
echo "To install system-wide:"
echo "  sudo make install"
echo
echo "For more build options:"
echo "  make help"
echo
echo "To build debug version:"
echo "  ./build.sh debug"