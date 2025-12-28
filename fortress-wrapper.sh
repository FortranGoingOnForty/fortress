#!/usr/bin/env bash
# FORTRESS wrapper script
# Installed to /usr/bin/fortress, calls binary at /usr/lib/fortress/fortress
#
# Note: This script cannot change your shell's directory. For cd-on-exit
# functionality (press 'c' to navigate), source fortress.sh in your shell rc:
#   source /usr/share/fortress/fortress.sh
#
# The shell function will take precedence over this script when sourced.

if [ -n "$FORTRESS_BIN" ] && [ -x "$FORTRESS_BIN" ]; then
    exec "$FORTRESS_BIN" "$@"
elif [ -x "/usr/lib/fortress/fortress" ]; then
    exec /usr/lib/fortress/fortress "$@"
elif [ -x "/usr/lib64/fortress/fortress" ]; then
    exec /usr/lib64/fortress/fortress "$@"
else
    echo "fortress: binary not found" >&2
    echo "Set FORTRESS_BIN to override the binary location." >&2
    exit 1
fi
