# FORTRESS shell integration
# Add this to your .bashrc or .zshrc:
#   source /usr/share/fortress/fortress.sh
#
# Then use: fortress
#
# This provides the cd-on-exit feature. Without sourcing this file,
# you can still run fortress but 'c' won't change your shell's directory.

fortress() {
    # Determine which fortress binary to use
    local fortress_exe

    # Check standard install locations (lib path first, then legacy bin path)
    if [ -x "/usr/lib/fortress/fortress" ]; then
        fortress_exe="/usr/lib/fortress/fortress"
    elif [ -x "/usr/lib64/fortress/fortress" ]; then
        fortress_exe="/usr/lib64/fortress/fortress"
    elif command -v fortress-bin &> /dev/null; then
        # Legacy: fortress-bin in PATH (Homebrew, older packages)
        fortress_exe="fortress-bin"
    elif [ -n "$FORTRESS_BIN" ]; then
        # Allow override via environment variable
        fortress_exe="$FORTRESS_BIN"
    elif [ -n "$FORTRESS_DIR" ]; then
        # Development: FORTRESS_DIR points to repo root
        fortress_exe="$FORTRESS_DIR/build/gfortran_"*"/app/fortress"
    else
        # Fallback: look for any fortress executable (NixOS, custom installs)
        # Use command -v to find it, but filter out this function
        local found
        found=$(type -P fortress 2>/dev/null)
        if [ -n "$found" ] && [ -x "$found" ]; then
            fortress_exe="$found"
        else
            echo "fortress: binary not found. Set FORTRESS_BIN or FORTRESS_DIR." >&2
            return 1
        fi
    fi

    # Run fortress
    "$fortress_exe" "$@"
    local exit_code=$?

    # Check if fortress wants us to cd somewhere
    if [ -f "$HOME/.fortress_cd" ]; then
        local target_dir
        target_dir=$(cat "$HOME/.fortress_cd")
        rm -f "$HOME/.fortress_cd"
        if [ -n "$target_dir" ] && [ -d "$target_dir" ]; then
            cd "$target_dir" || return 1
            echo "fortress: changed directory to $(pwd)"
        fi
    fi

    return $exit_code
}

# For zsh compatibility
if [ -n "$ZSH_VERSION" ]; then
    # Nothing special needed for zsh
    :
fi

# Export for subshells if needed (bash only)
if [ -n "$BASH_VERSION" ]; then
    export -f fortress 2>/dev/null || true
fi
