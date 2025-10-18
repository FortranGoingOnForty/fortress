#!/bin/bash

echo "========================================"
echo "         FORTRESS File Manager          "
echo "        Written in Modern Fortran       "
echo "========================================"
echo
echo "Features:"
echo "  • Dual-pane interface (parent | current)"
echo "  • Full keyboard navigation"
echo "  • Directory traversal"
echo "  • Clean, responsive display"
echo
echo "Controls:"
echo "  ↑/↓ or j/k : Navigate files"
echo "  → or l     : Enter directory"
echo "  ← or h     : Go to parent"
echo "  q          : Quit"
echo
echo "Building FORTRESS..."
fpm build --flag "-O2" 2>&1 | grep -E "(done|error|Error|ERROR)" || true

# Find the built executable
FORTRESS_BIN=$(find ./build -name fortress -type f -path "*/app/*" 2>/dev/null | head -1)

if [ -n "$FORTRESS_BIN" ] && [ -f "$FORTRESS_BIN" ]; then
    echo
    echo "Starting FORTRESS..."
    echo "===================="
    $FORTRESS_BIN
else
    echo "Build failed. Please run 'fpm build' to see full error messages."
    exit 1
fi