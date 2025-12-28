# FORTRESS shell integration for Fish
# This file should be placed in:
#   ~/.config/fish/functions/fortress.fish
# or
#   /usr/share/fish/vendor_functions.d/fortress.fish (system-wide)
#
# This provides the cd-on-exit feature. Without this function,
# you can still run fortress but 'c' won't change your shell's directory.

function fortress --description "Navigate filesystem with FORTRESS and cd on exit"
    set -l fortress_exe

    # Check standard install locations (lib path first, then legacy bin path)
    if test -x /usr/lib/fortress/fortress
        set fortress_exe /usr/lib/fortress/fortress
    else if test -x /usr/lib64/fortress/fortress
        set fortress_exe /usr/lib64/fortress/fortress
    else if command -v fortress-bin &> /dev/null
        # Legacy: fortress-bin in PATH (Homebrew, older packages)
        set fortress_exe fortress-bin
    else if set -q FORTRESS_BIN
        # Allow override via environment variable
        set fortress_exe $FORTRESS_BIN
    else if set -q FORTRESS_DIR
        # Development: FORTRESS_DIR points to repo root
        set fortress_exe $FORTRESS_DIR/build/gfortran_*/app/fortress
    else
        # Fallback: look for any fortress executable (NixOS, custom installs)
        set -l found (type -P fortress 2>/dev/null)
        if test -n "$found" -a -x "$found"
            set fortress_exe $found
        else
            echo "fortress: binary not found. Set FORTRESS_BIN or FORTRESS_DIR." >&2
            return 1
        end
    end

    # Run fortress
    eval $fortress_exe $argv
    set -l exit_code $status

    # Check if fortress wants us to cd somewhere
    if test -f $HOME/.fortress_cd
        set -l target_dir (cat $HOME/.fortress_cd)
        rm -f $HOME/.fortress_cd
        if test -n "$target_dir" -a -d "$target_dir"
            if cd $target_dir 2>/dev/null
                echo "fortress: changed directory to "(pwd)
            else
                echo "fortress: failed to change directory to $target_dir" >&2
            end
        end
    end

    return $exit_code
end
