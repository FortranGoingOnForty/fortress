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

- ✅ **Dual-pane display** inspired by Ranger/MC (parent dir 30% | current dir 70%)
- ✅ **Real filesystem navigation** with directory reading
- ✅ **Smart selection memory** - remembers position when navigating
- ✅ **Visual hierarchy** - dimmed parent pane, active current pane
- ✅ **Smooth updates** - no flashing, selective redraws
- ✅ **Arrow key and vim-style navigation** (h,j,k,l)
- ✅ **Directory visualization** with color coding (blue with `/` suffix)
- ✅ **Full-width selection bar** - clean highlighting
- ✅ **Navigate directories** - enter/exit with arrow keys

## Next Steps

- [ ] File opening with $EDITOR (Enter on files)
- [ ] FZF integration for fuzzy search (Ctrl-F)
- [ ] File operations (copy, move, delete)
- [ ] Configuration file support

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