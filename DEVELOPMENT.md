# PISSM Development Guide

## Architecture Overview

PISSM is organized as a modular Fortran application with clear separation of concerns:

### Module Hierarchy

```
constants_mod       ← All global constants
    ↑
types_mod          ← Custom data types
    ↑
globals_mod        ← Global variables (uses types_mod)
    ↑
utilities           ← General utility functions (uses constants_mod)
file_operations     ← File/directory operations (uses types_mod, utilities)
directory_tree      ← Directory tree management (uses types_mod, file_operations)
user_input          ← User input handling (uses constants_mod, utilities)
editor_display      ← UI display functions (uses types_mod, utilities)
editor_core         ← Core editor logic
interface_manager   ← Main UI logic (uses all above)
    ↑
main.f90           ← Entry point
```

### Module Responsibilities

#### `constants_mod`
- Global configuration values
- ANSI color codes
- File type icons
- Menu identifiers
- Screen dimensions

#### `types_mod`
- `file_entry`: Individual file metadata
- `directory_node`: Tree structure for directories
- `interface_state`: Current UI/selection state

#### `globals_mod`
- Global state variables
- Directory tree pointers
- Program control flags

#### `utilities`
- Terminal control (cursor, clear, colors)
- File information queries
- String formatting and manipulation
- Display helpers (headers, footers)
- Terminal setup/restore

#### `file_operations`
- Directory scanning with `ls` command
- File/directory copy, move, delete operations
- ls output parsing
- File attribute reading

#### `directory_tree`
- Directory tree creation and navigation
- File caching and refresh
- Search within directory tree

#### `user_input`
- Keyboard input handling
- Arrow key detection
- String/integer input prompts
- Signal handling

#### `editor_display`
- File list rendering
- File information display
- Menu rendering
- Status messages

#### `editor_core`
- File selection and highlighting
- Navigation logic
- View mode management
- Clipboard operations

#### `interface_manager`
- Main event loop
- User command handling
- State management
- Operation coordination

## Key Design Patterns

### 1. Directory Tree Navigation

The application maintains a tree structure of directories for efficient navigation:

```fortran
type(directory_node) :: current_node
type(directory_node), pointer :: root_node
```

Each node contains:
- Directory path
- Array of file entries
- File count
- Parent/children pointers
- Expansion state

### 2. State Management

UI state is centralized in `interface_state`:

```fortran
type(interface_state) :: ui_state
```

This includes:
- Current selection index
- Scroll position
- Display mode
- Hidden file visibility
- Clipboard path and operation
- Navigation history

### 3. String Handling

Fixed-length strings are used throughout for predictability:

```fortran
character(len=MAX_PATH_LENGTH)  :: path          ! 256 chars
character(len=MAX_FILENAME)     :: name          ! 64 chars
```

String limits defined in `constants_mod` prevent buffer overflows.

### 4. File Operations via Shell Commands

File operations use shell commands for portability:

```fortran
call execute_command_line('ls -la "path" > /tmp/file')
```

**Important**: All paths must be quoted to handle special characters.

## Temporary Files

The application uses `/tmp/` for:
- Terminal size detection: `/tmp/pissm_cols`, `/tmp/pissm_lines`
- Directory listings: `/tmp/pissm_ls_output`
- General temporary storage

**Cleanup**: Always remove temporary files explicitly.

## Error Handling

Current approach:
1. **File operations**: Check `iostat` on open/read/write
2. **System commands**: Check `exitstat` from `execute_command_line`
3. **Input validation**: Check string lengths before use
4. **Fallback values**: Provide sensible defaults (80x24 terminal, etc.)

**Future improvements**:
- Central error logging module
- Error recovery strategies
- User-friendly error messages
- Error history/logging

## Performance Considerations

### Current Limitations

1. **File listing**: Uses `ls` command every time, no caching
2. **String operations**: Fixed-length strings waste memory for short strings
3. **Terminal I/O**: ANSI codes sent for each display element
4. **Memory**: Limited to `MAX_FILES=50` per directory

### Optimization Opportunities

1. Implement file caching with invalidation
2. Batch terminal I/O operations
3. Dynamic string handling
4. Parallel file operations for copy/move
5. Progressive directory loading for large directories

## Adding New Features

### Step 1: Plan the Feature

- Where does it fit in the module hierarchy?
- What new data structures are needed?
- How will it interact with existing code?

### Step 2: Update Data Types

If needed, add fields to `file_entry`, `directory_node`, or `interface_state`:

```fortran
type :: interface_state
    ! ... existing fields ...
    logical :: new_feature_flag = .false.
end type interface_state
```

### Step 3: Implement Core Logic

Add functions/subroutines to appropriate modules:

```fortran
subroutine handle_new_feature()
    ! Implement feature logic
end subroutine handle_new_feature
```

### Step 4: Integrate into Main Loop

Add case statement in `main_interface_loop()`:

```fortran
case ('x', 'X')  ! New key
    call handle_new_feature()
    need_refresh = .true.
```

### Step 5: Add Display Support

Update display code if user interaction needed:

```fortran
call print_colored('Feature status: active', COLOR_GREEN)
```

### Step 6: Test Thoroughly

- Build with debug flags: `./build.sh debug`
- Test edge cases
- Verify terminal cleanup
- Check memory usage

## Common Tasks

### Adding a New Command Key

1. Identify which key should trigger the action
2. Add case statement in `interface_manager.f90`:
   ```fortran
   case ('x')
       call handle_x_command()
       need_refresh = .true.
   ```
3. Implement handler subroutine with appropriate error handling

### Adding a New Utility Function

1. Add to `utilities.f90` with documentation
2. Declare in public interface
3. Update module header comments
4. Add to appropriate existing module or create new one if substantial

### Improving File Operations

1. Current approach: shell commands
2. Consider: Direct Fortran system calls
3. See `file_operations.f90` for current implementation
4. Test on multiple systems

### Terminal Compatibility

1. ANSI escape sequences should work on most terminals
2. Test on: xterm, gnome-terminal, konsole, macOS terminal
3. Provide fallback for unsupported sequences
4. Document compatibility notes in README

## Debugging Tips

### Useful Compiler Flags (already in debug build)

```fortran
-g              ! Include debugging symbols
-O0             ! No optimization (clearer debugging)
-fcheck=all     ! Runtime bounds checking
-fbacktrace     ! Print backtrace on crash
```

### Running Under Debugger

```bash
gdb ./bin/pissm-debug
(gdb) run /tmp    ! Run with argument
(gdb) bt          ! Show backtrace on crash
```

### Common Issues

1. **Terminal corruption**: Ensure `restore_terminal()` is called on exit
2. **File not found**: Check path handling and quoting
3. **String overflow**: Verify array sizes and string lengths
4. **Memory issues**: Check allocation/deallocation pairs

## Testing Strategy

### Manual Testing Checklist

- [ ] Build succeeds without warnings
- [ ] Navigation works (j, k, gg, G)
- [ ] File operations (copy, move, delete)
- [ ] Search functionality works
- [ ] Terminal cleanup on exit
- [ ] Arrow keys work properly
- [ ] Special characters in filenames handled
- [ ] Large directories don't crash
- [ ] Hidden file toggle works
- [ ] Color output displays correctly

### Automated Testing (Future)

Candidates for unit testing:
- `parse_enhanced_ls_line()`: Parsing test data
- `format_size()`: Size conversion tests
- `to_lowercase()`: String conversion tests
- `is_directory()`: Path type detection tests

## Release Checklist

- [ ] All new features documented
- [ ] Compiler warnings resolved
- [ ] Debug build tested
- [ ] Release build created and tested
- [ ] CHANGELOG updated
- [ ] Version number incremented
- [ ] Binary size checked
- [ ] README updated if needed

## Resources for Contributors

- Fortran 2008 reference
- gfortran manual
- POSIX shell reference
- ANSI/VT100 escape sequences
- Design patterns in Fortran

## Future Roadmap

Proposed features and improvements:

### Version 1.1
- [ ] File editing (basic text editor mode)
- [ ] Undo/redo for file operations
- [ ] Favorites/bookmarks
- [ ] Sort options (by name, size, date)

### Version 1.2
- [ ] Recursive size calculation
- [ ] File permissions editor
- [ ] Archive browsing/extraction
- [ ] Multiple file selection

### Version 2.0
- [ ] Plugin system
- [ ] Themes
- [ ] Scripting support
- [ ] Network filesystem support

## Getting Help

- Check existing GitHub issues
- Review similar implementations in the codebase
- Consult Fortran documentation
- Ask in pull request comments
- Open a discussion issue

---

Happy coding! 🎉
