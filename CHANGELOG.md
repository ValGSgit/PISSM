# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

#### Documentation
- Added comprehensive `CONTRIBUTING.md` guide for developers
- Added `DEVELOPMENT.md` with architecture and design patterns
- Added `CODE_OF_CONDUCT.md` for community standards
- Added module-level documentation to all Fortran modules
- Added Doxygen-compatible documentation comments
- Improved inline code comments with explanations

#### Build System
- Enhanced `Makefile` with multiple targets:
  - `make release` - Optimized build with symbol stripping
  - `make debug` - Build with debugging symbols and runtime checks
  - `make clean` - Remove object files
  - `make distclean` - Remove all artifacts
  - `make install` - System-wide installation to `/usr/local/bin`
  - `make uninstall` - Remove system installation
  - `make check` - Syntax checking only
  - `make help` - Display help information
- Improved `build.sh` script:
  - Support for `debug` and `release` build types
  - Better error messages and status output
  - Cleaner build process using make
  - Improved help documentation
  - Support for custom Fortran compiler via `FC` environment variable

#### Project Setup
- Added comprehensive `.gitignore` for Fortran projects
- Includes patterns for build artifacts, IDE files, editor backups, and temporary files

### Improved

#### Code Quality
- Enhanced `utilities.f90` with:
  - Module-level documentation with Doxygen format
  - Function/subroutine documentation
  - Explicit public interface declaration
  - Improved code organization
  - Better parameter descriptions

- Enhanced `constants_mod.f90` with:
  - Comprehensive module header with description
  - Organized color codes into logical sections
  - Detailed constant descriptions

- Enhanced `types_mod.f90` with:
  - Detailed type documentation
  - Field-by-field descriptions
  - Clear purpose statements

- Enhanced `globals_mod.f90` with:
  - Module header with description
  - Clear documentation of each global variable
  - Purpose and usage notes

#### Development Experience
- Better build output with color-coded messages
- More helpful error messages for missing dependencies
- Clear instructions for installation and usage
- Support for both optimized and debugging builds
- Compiler warning flags enabled in build

#### Error Handling
- Improved terminal size detection with better fallbacks
- Better documentation of error conditions
- More robust temporary file cleanup

### Changed

- Build system now uses make for dependency management (build.sh delegates to make)
- Fortran compiler flags organized by optimization level
- Default Makefile flags changed to `-std=f2008 -Wall -Wextra -pedantic`
- Enhanced build script with better status messages

### Performance

- Added `-s` flag to release builds for symbol stripping (smaller binary)
- Separate debug and release compilation targets
- No performance regression; optimizations maintained

### Security

- Proper handling of user input in shell commands with quoting
- Improved error handling for file operations
- Better validation of file paths

## [Current Release]

This represents the current state of the project before improvements.

### Features

- File browser with vim-like navigation
- ANSI color terminal UI
- Directory tree navigation
- File operations (copy, move, delete)
- Search functionality
- Hidden file toggle
- Detailed file information display
- Breadcrumb navigation

### Known Limitations

- Limited to 50 files per directory (can be increased in constants_mod)
- Uses temporary files for terminal size detection
- File operations via shell commands
- No undo/redo for file operations
- Editor mode not yet functional (as noted in README)

---

## Future Planned Features

### Version 1.1
- [ ] File editing (basic text editor mode)
- [ ] Undo/redo for file operations  
- [ ] Favorites/bookmarks
- [ ] Sorting options (by name, size, date)

### Version 1.2
- [ ] Recursive size calculation
- [ ] File permissions editor
- [ ] Archive browsing
- [ ] Multiple file selection

### Version 2.0
- [ ] Plugin system
- [ ] Custom themes
- [ ] Scripting support
- [ ] Network filesystem support

---

## Contributing

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on how to contribute.

## License

See LICENSE file for details.
