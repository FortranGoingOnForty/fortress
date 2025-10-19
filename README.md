# FORTRESS
(noun) : all your base are belong to us

A command-line file explorer written in modern Fortran with fzf integration and git binds for fast staging and committing.

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

### Homebrew
```bash
brew tap FortranGoingOnForty/fortress
brew install fortress
```
Follow the caveats or quick directory jump won't work!

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

## Features

### File Explorer
- **Dual-pane display** inspired by Ranger/MC (parent dir 30% | current dir 70%)
- **Real filesystem navigation** with directory reading
- **Smart selection memory** - remembers position when navigating
- **Visual hierarchy** - dimmed parent pane, active current pane
- **Color-coded files**:
  - Directories: Blue + bold
  - Executable files: Green
  - Dotfiles: Grey
  - Regular files: White
- **Smooth scrolling** - viewport follows cursor automatically
- **Fuzzy finding with fzf** - press 'f' to search recursively and jump to files
- **CD on exit** - press 'c' to navigate your shell to selected directory

### Git Integration (inspired by fuss)
- **Git status indicators**:
  - `↑` (green) = Staged
  - `✗` (red) = Modified/unstaged
  - `✗` (grey) = Untracked
- **Repo name in status bar** - shows current repo name
- **Quick staging** - press 'A' to git add selected file
- **Interactive commits** - press 'M' for commit message prompt
- **Real-time updates** - indicators refresh after staging

## Controls

### Navigation
- `↑/↓`: Navigate up/down
- `→`: Enter directory
- `←`: Go back to parent directory
- `f`: Fuzzy find files with fzf (searches recursively)
- `c`: CD to selected directory and exit (requires shell integration)
- `q`: Quit

### Git Commands (when in a git repository)
- `A`: Stage selected file (git add)
- `U`: Unstage selected file (git restore --staged)
- `M`: Commit with message prompt (git commit -m)

## License

MIT
