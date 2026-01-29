#!/bin/sh

[ -f ~/.xprofile ] && . ~/.xprofile

sysresources="/home/devid/.Xresources"
sysmodmap="/home/devid/.xmodmap"

[ -f "$sysresources" ] && xrdb -merge "$sysresources"
[ -f "$sysmodmap" ] && xmodmap "$sysmodmap"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xset b off
xset s off
xset s noblank
xset r rate 280 80

( pactl set-sink-mute @DEFAULT_SINK@ 1 || true ) &
( pactl list short sources | grep -vi "monitor" | awk '{print $1}' \
    | xargs -r -I{} sh -c 'pactl set-source-mute {} 1; pactl set-source-volume {} 0%' || true ) &

brightnessctl -q set 20%

unclutter -idle 1 &

#xidlehook --not-when-audio \
#  --timer 2500 'xtrlock' '' \
#  --timer 500 'loginctl suspend' '' &

TP_ID="$(xinput list | awk -F'id=' '/[Tt]ouchpad/{print $2}' | awk '{print $1; exit}')"
[ -n "$TP_ID" ] && xinput disable "$TP_ID"


if xrandr --query | grep -q "HDMI-1 connected"; then
    xrand9
else
    xranddef
fi

exec xmonad

