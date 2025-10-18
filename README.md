# FORTRESS

A command-line file explorer written in modern Fortran with fzf integration.

## Installation

### From AUR (Arch Linux)

```bash
yay -S fortress
# or
paru -S fortress
```

Shell integration is automatically set up for bash and fish. Zsh users need to add to `~/.zshrc`:
```bash
source /usr/share/fortress/fortress.sh
```

### From Source

#### Prerequisites

- gfortran 10+ or ifort
- fpm (Fortran Package Manager)

### Install fpm

```bash
# Using cargo (if you have Rust)
cargo install fpm

# Or download from GitHub releases
# https://github.com/fortran-lang/fpm/releases
```

### Build & Run

```bash
# Build the project
fpm build

# Run FORTRESS
fpm run

# Or build and run in one command
fpm run --flag "-O2"
```

### Shell Integration (Optional)

To enable the "cd on exit" feature (press 'c' to navigate your shell to a directory):

```bash
# Add to your .bashrc or .zshrc:
source /path/to/fortress/fortress.sh

# Then use:
fortress  # instead of 'fpm run'
```

This allows you to navigate to directories and have your shell follow when you press 'c'.

### Development

```bash
# Run tests
fpm test

# Build with debug flags
fpm build --flag "-g -Wall -Wextra"
```

## Current Features

- ✅ **Dual-pane display** inspired by Ranger/MC (parent dir 30% | current dir 70%)
- ✅ **Real filesystem navigation** with directory reading
- ✅ **Smart selection memory** - remembers position when navigating
- ✅ **Visual hierarchy** - dimmed parent pane, active current pane
- ✅ **Color-coded files**:
  - Directories: Blue + bold
  - Executable files: Green
  - Dotfiles: Grey
  - Regular files: White
- ✅ **Smooth updates** - no flashing, selective redraws
- ✅ **Arrow key navigation** for intuitive movement
- ✅ **Full-width selection bar** - clean highlighting
- ✅ **CD on exit** - press 'c' to navigate your shell to selected directory

## Next Steps

- [ ] File opening with $EDITOR (Enter on files)
- [ ] FZF integration for fuzzy search (Ctrl-F)
- [ ] File operations (copy, move, delete)
- [ ] Configuration file support

## Roadmap

See [ROADMAP.md](ROADMAP.md) for detailed development plans.

## Controls

- `↑/↓`: Navigate up/down
- `→`: Enter directory
- `←`: Go back to parent directory
- `c`: CD to selected directory and exit (requires shell integration)
- `q`: Quit

## Architecture

FORTRESS is built as a self-contained file explorer:

- `app/main.f90`: Main application with integrated UI and file operations
- `lib_modules/`: Modular components (available for future expansion)
  - `filesystem/`: File operations and directory walking
  - `terminal/`: Terminal I/O and screen management
  - `ui/`: User interface components (panes, rendering)

## Packaging

For AUR maintainers, see [AUR_PACKAGING.md](AUR_PACKAGING.md) for complete packaging instructions.

## License

MIT