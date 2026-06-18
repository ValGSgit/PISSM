# Contributing to PISSM

Thank you for your interest in contributing to PISSM (Personal Interface System Structure & Modifications)! This document provides guidelines and instructions for developers.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Building the Project](#building-the-project)
- [Code Standards](#code-standards)
- [Making Changes](#making-changes)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Fortran Guidelines](#fortran-guidelines)
- [Documentation](#documentation)

## Code of Conduct

Please treat all contributors with respect. We aim to create a welcoming and inclusive community.

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/your-username/PISSM.git
   cd PISSM
   ```
3. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

## Development Setup

### Prerequisites

- **gfortran** (GNU Fortran compiler, version 4.8+)
- **GNU Make**
- **Linux/Unix environment** (macOS supported with gfortran via Homebrew)
- **git** for version control

### Installation

On Ubuntu/Debian:
```bash
sudo apt install gfortran make git
```

On macOS with Homebrew:
```bash
brew install gcc
```

On RHEL/CentOS:
```bash
sudo yum install gcc-gfortran make git
```

## Building the Project

### Standard Build

```bash
cd pissm
./build.sh              # Build release version
./build.sh debug        # Build with debugging symbols
make help               # Show all make targets
```

### Build Output

- **Release binary**: `pissm/bin/pissm` (optimized, stripped)
- **Debug binary**: `pissm/bin/pissm-debug` (with symbols and runtime checks)
- **Object files**: `pissm/obj/` (intermediate compilation)

### Clean Build

```bash
./build.sh clean    # Remove all build artifacts
make distclean       # Alternative: remove all artifacts including bin/
```

## Code Standards

### Fortran Style Guidelines

1. **Module organization**:
   - One logical module per file
   - Name files matching module names: `module_name_mod.f90`
   - Include module headers with description

2. **Naming conventions**:
   - **Modules**: `snake_case_mod` (e.g., `utilities_mod`)
   - **Programs**: `lowercase` (e.g., `pissm`)
   - **Variables**: `snake_case` (e.g., `file_count`)
   - **Constants**: `UPPER_CASE` (e.g., `MAX_FILES`)
   - **Subroutines**: `snake_case` (e.g., `clear_screen`)
   - **Functions**: `snake_case` (e.g., `file_exists`)

3. **Code formatting**:
   - Use 4 spaces for indentation (no tabs)
   - Keep lines under 100 characters when practical
   - Use meaningful variable names
   - Comment complex logic

4. **Documentation**:
   - Add module-level descriptions
   - Document subroutine/function purpose
   - Use Fortran 2008 free form
   - Include parameter descriptions

### Example Function

```fortran
!> Check if path is a directory
!!
!! Attempts to verify if the given path refers to a directory
!! using the system test command.
!!
!! \param[in] path      The file path to check
!! \return              True if path is a directory
function is_directory(path) result(is_dir)
    character(len=*), intent(in) :: path
    logical :: is_dir
    character(len=512) :: command
    integer :: status
    
    command = 'test -d "'//trim(path)//'"'
    call execute_command_line(command, exitstat=status)
    is_dir = (status == 0)
end function is_directory
```

## Making Changes

### Workflow

1. **Create a feature branch**:
   ```bash
   git checkout -b feature/add-search-feature
   ```

2. **Make your changes**:
   - Keep commits focused and atomic
   - One feature per branch
   - Write descriptive commit messages

3. **Commit message format**:
   ```
   Brief description (50 chars or less)
   
   Longer explanation if needed, explaining why the change
   was made and what it improves. Keep lines under 72 chars.
   
   - Use bullet points for lists
   - Reference issues with #123 if applicable
   ```

4. **Test your changes**:
   ```bash
   cd pissm
   make debug          # Build with checks enabled
   ./bin/pissm-debug   # Run the debug version
   ```

### Best Practices

- **Compile frequently**: Use `make debug` during development
- **Enable all warnings**: Compiler flags include `-Wall -Wextra -pedantic`
- **Check for runtime errors**: Use debug build with `-fcheck=all`
- **Keep it simple**: Avoid unnecessary complexity
- **Reuse existing utilities**: Check utilities.f90 before adding new functions

## Testing

### Manual Testing

1. **Build debug version**:
   ```bash
   cd pissm
   ./build.sh debug
   ```

2. **Test basic functionality**:
   ```bash
   ./bin/pissm-debug /tmp   # Navigate to /tmp directory
   # Test navigation, file operations, search
   ```

3. **Test edge cases**:
   - Empty directories
   - Directories with special characters in filenames
   - Very deep directory trees
   - Files with large sizes

### Automated Testing (Future)

We're working on adding automated test suites. If you'd like to help set this up, please open an issue!

## Submitting Changes

### Pull Request Process

1. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create a pull request** on GitHub:
   - Use a descriptive title
   - Reference any related issues (#123)
   - Explain what changes you made and why
   - Include any relevant screenshots or test results

3. **Code review**:
   - Address reviewer feedback promptly
   - Keep the conversation respectful
   - Make requested changes in new commits (don't force-push unless asked)

4. **After merge**:
   - Delete your feature branch (locally and remotely)
   - Celebrate! 🎉

### PR Checklist

Before submitting, ensure:
- [ ] Code compiles without warnings
- [ ] Code follows style guidelines
- [ ] No hardcoded magic numbers (use constants)
- [ ] Comments are clear and helpful
- [ ] Commit messages are descriptive
- [ ] No unnecessary files added
- [ ] Tested on your system

## Fortran Guidelines

### Important Notes

1. **Module dependencies**:
   - Keep dependencies unidirectional (no circular dependencies)
   - Document module dependencies in comments

2. **Memory management**:
   - Use `allocatable` arrays appropriately
   - Deallocate when done (future: use smart cleanup)
   - Document allocation patterns

3. **String handling**:
   - Use fixed-length strings with constants (e.g., `MAX_PATH_LENGTH`)
   - Trim unnecessary whitespace: `trim()`, `adjustl()`
   - Be careful with string concatenation

4. **I/O operations**:
   - Use `newunit=` for dynamic unit numbers
   - Always check `iostat` on file operations
   - Close files explicitly
   - Use `/dev/null` for silencing output

5. **External commands**:
   - Use `execute_command_line()` for shell integration
   - Capture output to temporary files when needed
   - Clean up temporary files
   - Provide fallback behavior if command fails

### Common Patterns

**Checking file existence**:
```fortran
if (file_exists(filepath)) then
    ! file operations
end if
```

**Formatted file size**:
```fortran
character(len=20) :: size_str
call format_size(file_size, size_str)
write(*, '(A)') trim(size_str)
```

**Dynamic unit numbers**:
```fortran
integer :: unit_num
open(newunit=unit_num, file=filename, status='old', iostat=iostat)
if (iostat == 0) then
    ! read from unit_num
    close(unit_num)
end if
```

## Documentation

### Adding Documentation

1. **Module headers**: Use Fortran 2008 `!>` documentation style
2. **Function/subroutine headers**: Document purpose and parameters
3. **Complex logic**: Add inline comments explaining why
4. **Public interfaces**: Document in README or inline

### Building Documentation (Future)

We plan to support Doxygen documentation generation. Help is welcome!

## Questions or Need Help?

- **Check existing issues** for similar questions
- **Open a new issue** with clear description of problem
- **Be specific**: Include error messages, steps to reproduce
- **Provide context**: OS, compiler version, build commands used

## Project Structure

```
PISSM/
├── README.md              # Project overview
├── CONTRIBUTING.md        # This file
├── .gitignore            # Git ignore rules
└── pissm/
    ├── build.sh          # Build script
    ├── Makefile          # Build configuration
    ├── modules/          # Fortran modules
    │   ├── constants_mod.f90
    │   ├── types_mod.f90
    │   └── globals_mod.f90
    └── src/              # Source files
        ├── main.f90
        ├── utilities.f90
        └── ...
```

## Resources

- [Fortran 2008 Standard](https://wg5-astrophysics.by/files/f2008n1830.pdf)
- [gfortran Documentation](https://gcc.gnu.org/wiki/GFortranStandards)
- [Modern Fortran](https://www.manning.com/books/modern-fortran)

## License

By contributing to PISSM, you agree that your contributions will be licensed under the same license as the project.

---

Thank you for contributing to PISSM! 🎉
