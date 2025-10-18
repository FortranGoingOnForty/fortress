# Using FORTRESS with CD-on-Exit

## Setup (One Time)

Add this line to your `~/.bashrc` or `~/.zshrc`:

```bash
source ~/Documents/GithubOrgs/FortranGoingOnForty/fortress/fortress.sh
```

Then reload your shell:
```bash
source ~/.bashrc  # or source ~/.zshrc
```

## Usage

Instead of running `fpm run`, use the `fortress` command:

```bash
fortress
```

This runs the fortress file explorer. When you're browsing:

1. Use `↑↓` to navigate files/directories
2. Use `→` to enter a directory
3. Use `←` to go back
4. **Press `c` on any directory to exit fortress and CD your shell to that directory**
5. Press `q` to quit without changing directory

## How It Works

When you press `c`:
1. FORTRESS writes the selected directory path to `~/.fortress_cd`
2. FORTRESS exits
3. The shell function reads `~/.fortress_cd` and runs `cd` to that directory
4. The temp file is cleaned up

This is the same pattern used by tools like `fzf`.

## Important Notes

- ❌ `fpm run` does NOT enable cd-on-exit (it runs directly)
- ✅ `fortress` command DOES enable cd-on-exit (uses the shell function)
- You must source `fortress.sh` in your shell config for this to work
- This only works in interactive shells, not in scripts
