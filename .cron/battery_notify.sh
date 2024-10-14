#!/bin/bash

BATTERY_LEVEL=$(cat /sys/class/power_supply/BAT1/capacity)
BATTERY_STATUS=$(cat /sys/class/power_supply/BAT1/status)

if [ "$BATTERY_STATUS" == "Discharging" ]; then
    if [ "$BATTERY_LEVEL" -le 10 ]; then
        notify-send "Battery Critical" "Battery level is ${BATTERY_LEVEL}%!" -u critical -t 0
    elif [ "$BATTERY_LEVEL" -le 30 ]; then
        notify-send "Battery Low" "Battery level is ${BATTERY_LEVEL}%!" -u normal -t 0
    elif [ "$BATTERY_LEVEL" -le 50 ]; then
        notify-send "Battery Warning" "Battery level is ${BATTERY_LEVEL}%!" -u low -t 0
    fi
fi

