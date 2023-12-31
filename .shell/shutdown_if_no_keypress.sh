#!/bin/bash

# Set the timeout duration in seconds
timeout=2

# Get the list of input devices and find the keyboard device ID
keyboard_id=$(xinput list | grep -i 'AT Translated Set 2 keyboard' | sed -n 's/.*id=\([0-9]\+\).*/\1/p')

# Check if keyboard ID was found
if [ -z "$keyboard_id" ]; then
  echo "Keyboard not found."
  exit 1
fi

# Use xinput test to listen for keypress events
{
  xinput test $keyboard_id &
  pid=$!
  sleep $timeout
  kill $pid 2>/dev/null
} | grep -q "key press"

# Check the output of xinput test for keypress events
if [ $? -eq 0 ]; then
  echo "Key press detected. Aborting shutdown."
else
  echo "No key press detected. Shutting down..."
  shutdown -h now
fi

