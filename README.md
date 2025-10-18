# FORTRESS

A command-line file explorer written in modern Fortran with fzf integration.

## Quick Start

### Prerequisites

- gfortran 10+ or ifort
- fpm (Fortran Package Manager)
- fzf (for fuzzy finding features)

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

### Development

```bash
# Run tests
fpm test

# Build with debug flags
fpm build --flag "-g -Wall -Wextra"
```

## Current Features

- ✓ Basic dual-pane display (parent dir | current dir)
- ✓ Arrow key and vim-style navigation (h,j,k,l)
- ✓ Basic terminal control with ANSI escape codes
- ✓ Directory structure visualization

## In Progress

- [ ] Actual filesystem reading (currently using placeholder data)
- [ ] File opening with $EDITOR
- [ ] FZF integration for fuzzy search

## Roadmap

See [ROADMAP.md](ROADMAP.md) for detailed development plans.

## Controls

- `↑/↓` or `j/k`: Navigate files
- `←/→` or `h/l`: Navigate directories
- `Enter`: Open file/enter directory
- `q` or `Ctrl-Q`: Quit

## Architecture

FORTRESS is built with modular design:

- `terminal/`: Terminal I/O and screen management
- `filesystem/`: File operations and directory walking
- `ui/`: User interface components (panes, rendering)
- `integration/`: External tool integration (fzf)

## License

MIT