#!/bin/bash

profile=$(powerprofilesctl get)

case "$profile" in
    performance)
        text="P"
        class="performance"
        ;;
    balanced)
        text="B"
        class="balanced"
        ;;
    power-saver)
        text="p"
        class="powersave"
        ;;
esac

# Output JSON for Waybar
printf '{"text":"%s","class":"%s"}\n' "$text" "$class"
