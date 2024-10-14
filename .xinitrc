#!/bin/sh
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/home/devid/.xmodmap"

[ -f "$sysresources" ] && xrdb -merge "$sysresources"
[ -f "$sysmodmap" ] && xmodmap "$sysmodmap"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xbindkeys -f /home/devid/.xbindkeysrc &
xset r rate 280 80
xset s 190
unclutter -idle 1
dunst &
numlockx
xset b off
feh --bg-fill /home/devid/pictures/wallpapers/kde6Pata-dark.png &

if [ -x "$(command -v xmonad)" ]; then
   exec /home/devid/.config/xmonad/xmonad-x86_64-linux
fi

