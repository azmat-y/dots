#!/bin/bash

# Get current power profile
current=$(powerprofilesctl get)

# Cycle to next profile
case "$current" in
    power-saver)
        powerprofilesctl set balanced
        ;;
    balanced)
        powerprofilesctl set performance
        ;;
    performance)
        powerprofilesctl set power-saver
        ;;
    *)
        # Default to balanced if unknown
        powerprofilesctl set balanced
        ;;
esac
