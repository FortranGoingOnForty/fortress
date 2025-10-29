#!/bin/bash
# Test if RESET properly clears REVERSE mode

echo "Test 1: REVERSE followed by RESET (ESC[0m)"
printf '\033[7mREVERSED LINE\033[0m\n'
printf 'Normal line 1\n'
printf 'Normal line 2\n'

echo ""
echo "Test 2: REVERSE followed by explicit un-reverse (ESC[27m) then RESET"
printf '\033[7mREVERSED LINE\033[27m\033[0m\n'
printf 'Normal line 1\n'
printf 'Normal line 2\n'

echo ""
echo "Which test shows 'Normal line 1' and 'Normal line 2' without reverse video?"
