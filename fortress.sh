# FORTRESS shell integration
# Add this to your .bashrc or .zshrc:
#   source ~/Documents/GithubOrgs/FortranGoingOnForty/fortress/fortress.sh
#
# Then use: fortress

fortress() {
    # Determine which fortress binary to use
    local fortress_exe

    if [ -x "/usr/bin/fortress-bin" ]; then
        # Use system-installed binary (via package manager)
        fortress_exe="/usr/bin/fortress-bin"
    elif [ -n "$FORTRESS_DIR" ]; then
        # Use FORTRESS_DIR if set
        fortress_exe="$FORTRESS_DIR/build/gfortran_"*"/app/fortress"
    else
        # Fallback to local development path
        fortress_exe="$HOME/Documents/GithubOrgs/FortranGoingOnForty/fortress/build/gfortran_"*"/app/fortress"
    fi

    # Run fortress
    $fortress_exe

    # Check if fortress wants us to cd somewhere
    if [ -f "$HOME/.fortress_cd" ]; then
        local target_dir
        target_dir=$(cat "$HOME/.fortress_cd")
        rm -f "$HOME/.fortress_cd"
        cd "$target_dir" || return 1
        echo "fortress: changed directory to $(pwd)"
    fi
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
