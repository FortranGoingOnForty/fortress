#!/bin/bash
# Test ANSI color rendering similar to fortress

echo "Testing ANSI codes as fortress uses them:"
echo ""

# Test 1: Normal color + filename
printf '\033[34m\033[1mDirectory/\033[0m\n'
printf '\033[37mFile.txt\033[0m\n'
printf '\033[32mExecutable\033[0m\n'

echo ""
echo "Testing REVERSE (cursor highlight):"

# Test 2: Reverse video with color
printf '\033[7m\033[34m\033[1mDirectory/\033[0m\n'
printf '\033[7m\033[37mFile.txt\033[0m\n'

echo ""
echo "Testing sequence like fortress renders:"

# Test 3: Full sequence like display.f90 does
printf '\033[34m\033[1mdir1/\033[0m\n'
printf '\033[7m\033[34m\033[1mdir2/\033[0m <- This should be highlighted\n'
printf '\033[37mfile1.txt\033[0m\n'

echo ""
echo "If 'dir2/' above does NOT look different (reversed), that's the bug."
