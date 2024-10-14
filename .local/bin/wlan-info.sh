#!/bin/bash

SSID=$(iwgetid -r)
TruncatedSSID=$(echo $SSID | cut -c 1-9)

if [ -z "$SSID" ]; then
	echo "nihil"
else
	echo "$TruncatedSSID"
fi
