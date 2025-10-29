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
- **Fuzzy finding with fzf** - press 's' to search recursively and jump to files
- **CD on exit** - press 'c' to navigate your shell to selected directory
- **Dotfiles toggle** - press '.' to show/hide dotfiles

### File Operations
- **Move mode** - press 'v' to enter move mode, navigate to destination, 'v' to confirm, 'q' to cancel
- **Rename** - press 'n' to rename files or directories
- **Delete** - press 'r' to remove/delete with confirmation prompt
  - Shows yellow warning for directories
  - Requires explicit 'y' confirmation
  - Handles both files and directories recursively
- **Open files** - press 'o' to open with default application (respects $EDITOR/$VISUAL)
- **Clipboard operations** (vim-style):
  - **Copy** - press 'y' to yank (copy) file/directory to clipboard
  - **Cut** - press 'x' to cut file/directory to clipboard
  - **Paste** - press 'p' to paste:
    - On directory: pastes INTO that directory
    - On file: pastes NEXT TO that file (into current directory)
    - On ".": pastes into current directory
    - On "..": pastes into parent directory
  - Copy clipboard persists for multiple pastes
  - Cut clipboard auto-clears after paste
  - **Smart duplicate handling** - automatically appends `-1`, `-2`, etc. if destination exists
    - `USAGE.md` → `USAGE-1.md`, `USAGE-2.md`, etc.
    - Works with any file extension or directories
- **Works on files AND directories** - all operations support recursive directory handling

### Git Integration (inspired by fuss)
- **Recursive git status indicators**:
  - `↑` (green) = Staged changes
  - `✗` (red) = Modified/unstaged changes
  - `✗` (grey) = Untracked files
  - `↓` (yellow) = Incoming changes from remote
  - **Directories show indicators** if they contain dirty files at any depth
  - **Indicators persist** across subdirectory navigation
- **Batch operations** - stage/unstage entire directories recursively
- **Repo name and branch** in status bar
- **Full git workflow**:
  - Stage, unstage, commit, fetch, pull, push, tag
  - View diffs with color output
  - Incoming change detection
- **Real-time updates** - indicators refresh after git operations
- **Smart upstream handling** - interactive branch selection when no upstream configured

## Controls

### Navigation
- `↑/↓`: Navigate up/down
- `→`: Enter directory
- `←`: Go back to parent directory
- `~`: Jump to home directory
- `/`: Jump to root directory
- `s`: Search files with fzf (fuzzy find recursively)
- `c`: CD to selected directory and exit (requires shell integration)
- `q`: Quit (or exit move mode if active)
- `.`: Toggle dotfiles visibility

### File Operations
- `v`: Enter move mode / confirm move
- `y`: Yank (copy) to clipboard
- `x`: Cut to clipboard
- `p`: Paste from clipboard
- `n`: Rename file/directory
- `r`: Remove/delete file/directory (with confirmation)
- `o`: Open file with default application

### Git Commands (when in a git repository)
- `a`: Stage file/directory (batch stages directories recursively)
- `u`: Unstage file/directory (batch unstages directories recursively)
- `m`: Commit with message prompt
- `d`: Show git diff for selected file
- `f`: Fetch from remote
- `l`: Pull from remote
- `h`: Push to remote
- `t`: Create git tag

## Visual Feedback

FORTRESS provides clear visual feedback for all operations:

### Header Status
- **Normal**: Shows current directory path
- **Move mode**: Shows `MOVE: filename` in red
- **Copy clipboard**: Shows `COPY: filename` in green
- **Cut clipboard**: Shows `CUT: filename` in yellow

### Move Mode
When in move mode (press 'v'):
- Source file/directory appears in **red bold**
- Destination cursor has **white background**
- Footer shows move mode controls
- Press 'q' to cancel, 'v' to confirm

### Clipboard Visual Feedback
- **Copied files**: Normal appearance, shown in header as green `COPY: filename`
- **Cut files**: Appear in **dark red** (dimmed) to indicate they will be moved
- **Cut file selected**: Shows with **red background** instead of normal selection color
- Visual indication persists until paste operation completes

### Operation Feedback
All file operations (move, copy, cut, paste, rename) display:
- Success/failure status with ✓/✗ indicators
- Source and destination paths
- 2-second pause to review before returning to navigation

## License

MIT
