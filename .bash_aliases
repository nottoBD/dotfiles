# /etc/profile.d/
alias ..='cd ..'
alias ls='exa -al --color=always --group-directories-first'
alias grep='grep --color=auto'
alias clear='clear && pfetch'
alias cd='cd_func'

cd_func() {
	if [ "$#" -eq 0 ]; then
		builtin cd ~
	else
		builtin cd "$@" && [ "$(ls -1 | wc -l)" -lt 25 ] && exa -al --color=always --group-directories-first
	fi
}

alias xev='xev | grep keycode'
alias xprop='xprop | grep WM_CLASS'

alias vim='nvim'
alias vi='nvim'
alias Mirrors='sudo reflector --verbose --country "France,Germany,United Kingdom,Netherlands,Belgium,Luxembourg,Ireland" --age 12 --protocol https --sort rate --save /etc/pacman.d/mirrorlist --fastest 8'
alias Orphans='sudo pacman -Rns $(pacman -Qdtq)'
alias Recompile='ghc --make xmonad.hs -i -ilib -dynamic -fforce-recomp -o xmonad-x86_64-linux'
alias Shutdown='shutdown -h now'
alias Sized='sudo du -h / | grep "[0-9\.]\+G\|[5-9][0-9][0-9]M" | sort -rh'
