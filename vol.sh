#!/bin/bash
if [[ "$(amixer sget Master | awk -F\"[][]\" '/dB/ { print $2 }' | cut -c 1-3)" -ne "100" ]]; then printf 0; fi; amixer sget Master | awk -F"[][]" '/dB/ { print $2 }' | rev | cut -c 2- | rev 
