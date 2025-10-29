#!/bin/bash
# This demonstrates the proper usage

echo "==== FORTRESS CD-ON-EXIT TEST ===="
echo ""
echo "1. First, source the fortress function:"
echo "   source ~/Documents/GithubOrgs/FortranGoingOnForty/fortress/fortress.sh"
echo ""
echo "2. Then run: fortress"
echo ""
echo "3. Navigate to a directory you want to cd to"
echo ""
echo "4. Press 'c' on that directory"
echo ""
echo "5. Your shell should cd to that directory"
echo ""
echo "==== TRY THIS IN YOUR TERMINAL ===="
echo ""
echo "Current directory: $(pwd)"
echo ""

# Source fortress
source ~/Documents/GithubOrgs/FortranGoingOnForty/fortress/fortress.sh

# Simulate what happens when you press 'c' in fortress
echo "Simulating: you pressed 'c' on the docs/ directory..."
mkdir -p ~/Documents/GithubOrgs/FortranGoingOnForty/fortress/docs
echo "~/Documents/GithubOrgs/FortranGoingOnForty/fortress/docs" > ~/.fortress_cd

# This is what the fortress() function does after fortress exits
if [ -f "$HOME/.fortress_cd" ]; then
    target_dir=$(cat "$HOME/.fortress_cd")
    rm -f "$HOME/.fortress_cd"
    cd "$target_dir"
    echo "✓ Changed directory to: $(pwd)"
fi
