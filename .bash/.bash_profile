if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    xmonad --recompile && exec startx
fi

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if ! dinitctl --user list &>/dev/null; then
    setsid dinit --user </dev/null >/dev/null 2>&1 &
fi
eval "$(zoxide init bash)"
set -o vi




export PATH=$PATH:/home/devid/.spicetify
