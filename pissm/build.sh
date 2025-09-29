#!/bin/bash

# Build script for PISSM
# Personal Interface System Structure & Modifications

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

# Check if gfortran is available
if ! command -v gfortran &> /dev/null; then
    print_error "gfortran compiler not found. Please install gfortran."
    echo "On Ubuntu/Debian: sudo apt install gfortran"
    echo "On RHEL/CentOS: sudo yum install gcc-gfortran"
    echo "On macOS: brew install gcc"
    exit 1
fi

print_status "Starting PISSM build process..."

# Create directory structure
print_status "Creating directory structure..."
mkdir -p obj bin

# Check if all source files exist
print_status "Checking source files..."
required_files=(
    "modules/constants_mod.f90"
    "modules/types_mod.f90"
    "modules/globals_mod.f90"
    "src/utilities.f90"
    "src/file_operations.f90"
    "src/user_input.f90"
    "src/directory_tree.f90"
    "src/interface_manager.f90"
    "src/main.f90"
)

for file in "${required_files[@]}"; do
    if [ ! -f "$file" ]; then
        print_error "Required file missing: $file"
        exit 1
    fi
done

# Build the project
print_status "Building PISSM..."

# Set compiler flags
FFLAGS="-std=f2008 -Wall -Wextra -pedantic -fcheck=all -g -O2"

# Compile modules first
print_status "Compiling modules..."
gfortran $FFLAGS -J obj -c modules/constants_mod.f90 -o obj/constants_mod.o
gfortran $FFLAGS -J obj -c modules/types_mod.f90 -o obj/types_mod.o
gfortran $FFLAGS -J obj -c modules/globals_mod.f90 -o obj/globals_mod.o

# Compile source files
print_status "Compiling source files..."
gfortran $FFLAGS -J obj -c src/utilities.f90 -o obj/utilities.o
gfortran $FFLAGS -J obj -c src/file_operations.f90 -o obj/file_operations.o
gfortran $FFLAGS -J obj -c src/user_input.f90 -o obj/user_input.o
gfortran $FFLAGS -J obj -c src/directory_tree.f90 -o obj/directory_tree.o
gfortran $FFLAGS -J obj -c src/interface_manager.f90 -o obj/interface_manager.o
gfortran $FFLAGS -J obj -c src/main.f90 -o obj/main.o

# Link the executable
print_status "Linking executable..."
gfortran $FFLAGS obj/*.o -o bin/pissm

# Check if build was successful
if [ -f "bin/pissm" ]; then
    print_success "PISSM built successfully!"
    echo
    echo -e "${GREEN}🎉 PISSM - Personal Interface System Structure & Modifications ${GREEN}🎉${NC}"
    echo -e "${BLUE}=================================================================${NC}"
    echo -e "${BLUE}📁 Executable location:${NC} bin/pissm"
    echo -e "${BLUE}🚀 Run with:${NC} ./bin/pissm"
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
    print_error "Build failed!"
    exit 1
fi

# Make executable
chmod +x bin/pissm

print_success "Build completed successfully!"
echo
echo "To run PISSM:"
echo "  ./bin/pissm"
echo
echo "To install system-wide:"
echo "  sudo make install"
echo
echo "For help:"
echo "  ./bin/pissm and press '?' in the interface"