# ~/.config/fish/conf.d/20-aliases.fish

# helpers
alias .xmonad 'vim "$XMOCONF"'
alias .upload 'curl bashupload.com -T'
alias .xev 'xev | grep keycode'
alias .xprop 'xprop | grep WM_CLASS'
alias .ip 'curl -4 ifconfig.me'
alias .sshP "eval (ssh-agent -c); and set -x GPG_TTY (tty); and ssh-add ~/.ssh/id_ed25519_github"


# wrappers
alias casing 'xdotool key Caps_Lock'
alias cleanup 'pc -Sc'
alias journal 'journalctl -p 3 -b'
alias mount_pass '~/.local/bin/mount-password-store'
alias orphans 'pacman -Qdtq'
alias pc-integrity 'pacman -Qkk'
alias C 'xclip -selection clipboard'

# “cat → bat”, with escape hatch `c` to force real cat
alias c 'bat --number'

alias ls 'exa --color=always --icons -la --git'
alias tree 'exa --color=always --icons -a --tree'
alias grep 'grep --color=auto'
alias halt 'poweroff'
alias lsblk 'lsblk -o NAME,SIZE,FSTYPE,LABEL,MOUNTPOINT,UUID,MODEL,SERIAL'
alias nano 'vim'
alias paru 'yay'
alias pc 'sudo pacman'
alias st 'git status'
alias v 'nvim'
alias vi 'command vim'
alias vim 'nvim'
alias sys 'sudo dinitctl'

alias reboot "loginctl reboot"
alias poweroff "loginctl poweroff"
