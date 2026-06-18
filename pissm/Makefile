# Makefile for PISSM (Personal Interface System Structure & Modifications)

# Compiler settings
FC ?= gfortran
FFLAGS_BASE = -std=f2008 -Wall -Wextra -pedantic -J$(OBJDIR)
FFLAGS_RELEASE = $(FFLAGS_BASE) -O2 -s
FFLAGS_DEBUG = $(FFLAGS_BASE) -g -O0 -fcheck=all -fbacktrace
FFLAGS = $(FFLAGS_RELEASE)
LDFLAGS = 

# Directories
SRCDIR = src
MODDIR = modules
OBJDIR = obj
BINDIR = bin
DISTDIR = dist

# Target executable
TARGET = $(BINDIR)/pissm
TARGET_DEBUG = $(BINDIR)/pissm-debug

# Source files (ordered by dependencies)
MODSRCS = $(MODDIR)/constants_mod.f90 $(MODDIR)/types_mod.f90 $(MODDIR)/globals_mod.f90
SRCS = $(SRCDIR)/utilities.f90 $(SRCDIR)/file_operations.f90 $(SRCDIR)/directory_tree.f90 $(SRCDIR)/user_input.f90 $(SRCDIR)/editor_display.f90 $(SRCDIR)/editor_core.f90 $(SRCDIR)/interface_manager.f90 $(SRCDIR)/main.f90

# Object files
MODOBS = $(MODSRCS:$(MODDIR)/%.f90=$(OBJDIR)/%.o)
OBJS = $(SRCS:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

# Phony targets
.PHONY: all debug release clean distclean install uninstall help check

# Default target
all: release

# Release build (optimized, stripped)
release: FFLAGS = $(FFLAGS_RELEASE)
release: $(TARGET)
	@echo "✓ Release build complete: $(TARGET)"

# Debug build (with debugging symbols and runtime checks)
debug: FFLAGS = $(FFLAGS_DEBUG)
debug: $(TARGET_DEBUG)
	@echo "✓ Debug build complete: $(TARGET_DEBUG)"

# Create directories
$(OBJDIR):
	mkdir -p $(OBJDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

# Build release target
$(TARGET): $(OBJDIR) $(BINDIR) $(MODOBS) $(OBJS)
	$(FC) $(FFLAGS) $(MODOBS) $(OBJS) -o $(TARGET) $(LDFLAGS)

# Build debug target
$(TARGET_DEBUG): $(OBJDIR) $(BINDIR) $(MODOBS) $(OBJS)
	$(FC) $(FFLAGS) $(MODOBS) $(OBJS) -o $(TARGET_DEBUG) $(LDFLAGS)

# Compile module files
$(OBJDIR)/%.o: $(MODDIR)/%.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Compile source files
$(OBJDIR)/%.o: $(SRCDIR)/%.f90 $(MODOBS) | $(OBJDIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Clean build artifacts
clean:
	rm -rf $(OBJDIR)

# Full cleanup (including executables)
distclean: clean
	rm -rf $(BINDIR) $(DISTDIR)

# System-wide installation
install: release
	@echo "Installing PISSM to system..."
	mkdir -p $(DESTDIR)/usr/local/bin
	install -m 755 $(TARGET) $(DESTDIR)/usr/local/bin/pissm
	@echo "✓ Installation complete. Run 'pissm' from anywhere."

# Uninstall from system
uninstall:
	@echo "Removing PISSM from system..."
	rm -f $(DESTDIR)/usr/local/bin/pissm
	@echo "✓ Uninstallation complete."

# Check syntax only (fast compilation check)
check:
	@echo "Checking Fortran syntax..."
	$(FC) -fsyntax-only $(MODSRCS) $(SRCS)
	@echo "✓ Syntax check passed"

# Help target
help:
	@echo "PISSM Makefile targets:"
	@echo "  make              - Build release version (default)"
	@echo "  make release      - Build optimized release version"
	@echo "  make debug        - Build with debugging symbols and checks"
	@echo "  make clean        - Remove object files"
	@echo "  make distclean    - Remove all build artifacts"
	@echo "  make install      - Install to /usr/local/bin (requires sudo)"
	@echo "  make uninstall    - Remove from /usr/local/bin (requires sudo)"
	@echo "  make check        - Check Fortran syntax only"
	@echo "  make help         - Show this help message"