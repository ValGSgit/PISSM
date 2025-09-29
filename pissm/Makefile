# Makefile for PISSM (Personal Interface System Structure & Modifications)

# Compiler settings
FC = gfortran
FFLAGS = -std=f2008 -Wall -Wextra -pedantic -O2 -s  # Removed -fcheck=all and -g, added -s for strip symbols
LDFLAGS = -s 

# Directories
SRCDIR = src
MODDIR = modules
OBJDIR = obj
BINDIR = bin

# Target executable
TARGET = $(BINDIR)/pissm

# Source files (ordered by dependencies)
MODSRCS = $(MODDIR)/constants_mod.f90 $(MODDIR)/types_mod.f90 $(MODDIR)/globals_mod.f90
SRCS = $(SRCDIR)/utilities.f90 $(SRCDIR)/file_operations.f90 $(SRCDIR)/directory_tree.f90 $(SRCDIR)/user_input.f90 $(SRCDIR)/interface_manager.f90 $(SRCDIR)/main.f90

# Object files
MODOBS = $(MODSRCS:$(MODDIR)/%.f90=$(OBJDIR)/%.o)
OBJS = $(SRCS:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

# Default target
all: $(TARGET)

# Create directories
$(OBJDIR):
	mkdir -p $(OBJDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

# Build target
$(TARGET): $(OBJDIR) $(BINDIR) $(MODOBS) $(OBJS)
	$(FC) $(FFLAGS) $(MODOBS) $(OBJS) -o $(TARGET) $(LDFLAGS)

# Compile module files
$(OBJDIR)/%.o: $(MODDIR)/%.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -c $< -o $@

# Compile source files
$(OBJDIR)/%.o: $(SRCDIR)/%.f90 $(MODOBS) | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -c $< -o $@

# Clean build artifacts
clean:
	rm -rf $(OBJDIR) $(BINDIR)

# Install (copy to system directory)
install: $(TARGET)
	cp $(TARGET) /usr/local/bin/pissm
	chmod +x /usr/local/bin/pissm

# Uninstall
uninstall:
	rm -f /usr/local/bin/pissm

# Debug build
debug: FFLAGS += -DDEBUG -fbounds-check -fbacktrace
debug: $(TARGET)

# Run the program
run: $(TARGET)
	./$(TARGET)

# Help
help:
	@echo "Available targets:"
	@echo "  all      - Build the program (default)"
	@echo "  clean    - Remove build artifacts"
	@echo "  install  - Install to system directory"
	@echo "  uninstall- Remove from system directory"
	@echo "  debug    - Build with debug information"
	@echo "  run      - Build and run the program"
	@echo "  help     - Show this help message"

.PHONY: all clean install uninstall debug run help