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

## Controls

- `↑/↓`: Navigate up/down
- `→`: Enter directory
- `←`: Go back to parent directory
- `c`: CD to selected directory and exit (requires shell integration)
- `q`: Quit

## License

MIT
