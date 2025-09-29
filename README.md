# 🎉 PISSM - Personal Interface System Structure & Modifications

**An Enhanced File System Monitor with Beautiful Visual Interface**

## ✨ Features

### 🎨 Visual Interface
- **Beautiful color-coded interface** with emoji icons for file types
- **Enhanced readability** with carefully chosen colors and formatting  
- **Modern terminal UI** that's both functional and aesthetically pleasing
- **Responsive layout** that adapts to terminal size

### 📊 Detailed File Information
- **Complete file permissions** displayed with color coding
- **File sizes** automatically formatted (B, KB, MB, GB, TB)
- **Ownership information** (user.group)
- **Modification timestamps** 
- **File type detection** with appropriate icons
- **Detailed file info view** (press `i` key)

### ⚡ Navigation & Control
- **Vim-like keybindings** for efficient navigation
- **Fast directory traversal** with breadcrumb navigation
- **Quick search** within directories (`/` key)
- **Advanced search** across directory trees (`s` key)
- **Jump to first/last** items (`gg`/`G`)
- **Home directory shortcut** (`~` key)

### 🗂️ File Operations  
- **Copy/Move/Delete** files and directories
- **Create** new files and directories
- **Smart clipboard** management
- **Safe deletion** with confirmation prompts
- **Hidden files toggle** (`h` key)
- **Directory refresh** (`r` key)

### 🔍 Search Capabilities
- **Quick in-directory search** with `/` key
- **Recursive file search** with pattern matching  
- **Case-insensitive searching**
- **Search result navigation**

## 🚀 Quick Start

### Prerequisites
- **gfortran** compiler (GNU Fortran)
- **Linux/Unix system** (tested on Linux)
- **Terminal** supporting ANSI colors

### Installation

1. **Clone or download** the PISSM source code
2. **Navigate** to the pissm directory
3. **Build** the project:

```bash
# Using the build script (recommended)
./build.sh

# Or using make directly
make

# Or for debug build
make debug
```

4. **Run** PISSM:
```bash
./bin/pissm
```

### System-wide Installation
```bash
sudo make install
# Then run from anywhere with:
pissm
```

## 🎯 Keyboard Commands

### Navigation
| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `gg` | Go to first item |
| `G` | Go to last item |
| `Enter` | Open file/directory |
| `b` | Go back to parent directory |
| `~` | Go to home directory |

### View & Search
| Key | Action |
|-----|--------|
| `i` | Show detailed file information |
| `h` | Toggle hidden files |
| `r` | Refresh directory |
| `/` | Quick search in current directory |
| `s` | Advanced search (recursive) |

### File Operations
| Key | Action |
|-----|--------|
| `c` | Copy file to clipboard |
| `m` | Move file to clipboard |
| `p` | Paste from clipboard |
| `d` | Delete file/directory |
| `n` | Create new file or directory |

### System
| Key | Action |
|-----|--------|
| `?` | Show help screen |
| `q` | Quit PISSM |

## 🎨 Visual Features

### File Type Icons
- 📁 **Directories** - Blue colored names
- 📄 **Regular files** - White names
- ⚡ **Executable files** - Green names with bold formatting
- 🔗 **Symbolic links** - Cyan colored
- 📦 **Archives** (.zip, .tar.gz, etc.)
- 🖼️ **Images** (.jpg, .png, .gif, etc.)
- 💾 **Source code** (.c, .py, .f90, etc.)
- 📝 **Text files** (.txt, .md, .log, etc.)

### Permission Color Coding
- **Green** - Owner permissions (user)
- **Yellow** - Group permissions  
- **Red** - Other permissions
- **Dim** - No permission granted

### Interface Elements
- **Breadcrumb navigation** showing current path
- **Column headers** for organized information display
- **Status bar** with file count and current settings
- **Selection highlighting** with visual indicators
- **Enhanced borders** and separators

## 🛠️ Building from Source

### Manual Compilation
If you prefer to compile manually:

```bash
# Create directories
mkdir -p obj bin

# Compile modules first
gfortran -std=f2008 -Wall -J obj -c modules/constants_mod.f90 -o obj/constants_mod.o
gfortran -std=f2008 -Wall -J obj -c modules/types_mod.f90 -o obj/types_mod.o  
gfortran -std=f2008 -Wall -J obj -c modules/globals_mod.f90 -o obj/globals_mod.o

# Compile source files
gfortran -std=f2008 -Wall -J obj -c src/utilities.f90 -o obj/utilities.o
gfortran -std=f2008 -Wall -J obj -c src/file_operations.f90 -o obj/file_operations.o
gfortran -std=f2008 -Wall -J obj -c src/user_input.f90 -o obj/user_input.o
gfortran -std=f2008 -Wall -J obj -c src/directory_tree.f90 -o obj/directory_tree.o
gfortran -std=f2008 -Wall -J obj -c src/interface_manager.f90 -o obj/interface_manager.o
gfortran -std=f2008 -Wall -J obj -c src/main.f90 -o obj/main.o

# Link executable
gfortran obj/*.o -o bin/pissm
```

## 🐛 Troubleshooting

### Common Issues
1. **Compiler not found**: Install gfortran
   - Ubuntu/Debian: `sudo apt install gfortran`
   - RHEL/CentOS: `sudo yum install gcc-gfortran`
   - macOS: `brew install gcc`

2. **Permission denied**: Make sure build.sh is executable
   ```bash
   chmod +x build.sh
   ```

3. **Display issues**: Ensure your terminal supports ANSI colors
   - Most modern terminals support this
   - Try different terminal emulators if needed

4. **File access errors**: Check directory permissions
   - PISSM respects system file permissions
   - Some system directories may require elevated privileges

## 📋 Technical Details

### Memory Management
- **Proper cleanup** of dynamic memory allocation
- **Leak-free operation** with careful pointer management
- **Efficient directory scanning** with minimal memory footprint

### Performance 
- **Fast directory loading** using system commands
- **Responsive interface** with minimal input lag
- **Efficient file operations** with proper error handling

### Compatibility
- **Fortran 2008 standard** compliance
- **POSIX-compatible** system calls
- **Cross-platform** design (Linux/Unix focus)

## 🤝 Contributing

This project was enhanced with focus on:
- Visual appeal and user experience
- Memory safety and leak prevention  
- Feature completeness for daily file management
- Code organization and maintainability

Feel free to suggest improvements or report issues!

## 📜 License

This project builds upon the original PISSM codebase with significant enhancements for better usability, visual appeal, and functionality.

---

**Enjoy exploring your filesystem with PISSM! 🚀**
