#!/bin/bash

LOGFILE="/tmp/monitor_hotswap_debug.log"

echo "$(date): Starting monitor hotswap check" >> $LOGFILE
echo "DISPLAY: $DISPLAY" >> $LOGFILE
echo "XAUTHORITY: $XAUTHORITY" >> $LOGFILE

export DISPLAY=:0
export XAUTHORITY=/home/devid/.Xauthority

echo "After setting: DISPLAY: $DISPLAY, XAUTHORITY: $XAUTHORITY" >> $LOGFILE

xrandr >> $LOGFILE 2>&1

HDMI_STATUS=$(xrandr | grep "HDMI-1-0 connected")
echo "HDMI_STATUS: $HDMI_STATUS" >> $LOGFILE

if [ -n "$HDMI_STATUS" ]; then
    echo "HDMI is connected" >> $LOGFILE
    
    RESOLUTION=$(xrandr | grep -A1 "HDMI-1-0 connected" | tail -n 1 | awk '{print $1}')
    echo "Resolution: $RESOLUTION" >> $LOGFILE

    case "$RESOLUTION" in
        1680x1050)
            ~/.config/xrandr/top_down_1680x1050.sh
            echo "$(date): Applied top_down_1680x1050.sh" >> $LOGFILE
            ;;
        1920x1080)
            ~/.config/xrandr/top_down_1920x1080.sh
            echo "$(date): Applied top_down_1920x1080.sh" >> $LOGFILE
            ;;
        *)
            ~/.config/xrandr/defaults.sh
            echo "$(date): Applied defaults.sh" >> $LOGFILE
            ;;
    esac
else
    echo "HDMI is disconnected" >> $LOGFILE
    ~/.config/xrandr/defaults.sh
    echo "$(date): Applied defaults.sh for disconnection" >> $LOGFILE
fi

