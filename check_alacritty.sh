#!/bin/bash
# Script to check alacritty environment variables

echo "=== Terminal Environment Check ==="
echo "TERM=$TERM"
echo "ALACRITTY_SOCKET=$ALACRITTY_SOCKET"
echo "ALACRITTY_LOG=$ALACRITTY_LOG"
echo "ALACRITTY_WINDOW_ID=$ALACRITTY_WINDOW_ID"
echo ""
echo "All environment variables containing 'alacritty' or 'ALACRITTY':"
env | grep -i alacritty
