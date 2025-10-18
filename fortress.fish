# FORTRESS shell integration for Fish
# This file should be placed in:
#   ~/.config/fish/functions/fortress.fish
# or
#   /usr/share/fish/vendor_functions.d/fortress.fish (system-wide)

function fortress --description "Navigate filesystem with FORTRESS and cd on exit"
    # Set fortress directory - prefer system install, fallback to FORTRESS_DIR env var
    set -l fortress_dir
    if set -q FORTRESS_DIR
        set fortress_dir $FORTRESS_DIR
    else if test -x /usr/bin/fortress-bin
        # Use system-installed binary
        set fortress_exe /usr/bin/fortress-bin
    else
        # Fallback to local development path
        set fortress_dir $HOME/Documents/GithubOrgs/FortranGoingOnForty/fortress
        set fortress_exe $fortress_dir/build/gfortran_*/app/fortress
    end

    # Run fortress
    if test -n "$fortress_exe"
        eval $fortress_exe
    else
        $fortress_dir/build/gfortran_*/app/fortress
    end

    # Check if fortress wants us to cd somewhere
    if test -f $HOME/.fortress_cd
        set -l target_dir (cat $HOME/.fortress_cd)
        rm -f $HOME/.fortress_cd
        if cd $target_dir 2>/dev/null
            echo "fortress: changed directory to "(pwd)
        else
            echo "fortress: failed to change directory to $target_dir" >&2
        end
    end
end
