[[ $- != *i* ]] && return

if [ -f $HOME/.bash/prompt ]; then
	. $HOME/.bash/prompt
fi

if [ -f $HOME/.bash/aliases ]; then
	. $HOME/.bash/aliases
fi

if [ -f $HOME/.bash/functions ]; then
	. $HOME/.bash/functions
fi

if [ -f $HOME/.bash/exports ]; then
	. $HOME/.bash/exports
fi

if [ -f $HOME/.bash/paths ]; then
	. $HOME/.bash/paths
fi

if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

colorscript random

[ -f "$XDG_CONFIG_HOME/shell/aliases.sh" ] && . "$XDG_CONFIG_HOME/shell/aliases.sh"


export PATH=$PATH:/home/devid/.spicetify

# pnpm
export PNPM_HOME="/home/devid/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
