# /etc/bash.bashrc
[[ $- != *i* ]] && return

if [ -f ~/.bash/.bash_exports ]; then
	. ~/.bash/.bash_exports
fi

if [ -f ~/.bash/.bash_aliases ]; then
	. ~/.bash/.bash_aliases
fi

if [ -f ~/.bash/.bash_functions ]; then
	. ~/.bash/.bash_functions
fi

if [ -f ~/.bash/.bash_prompt ]; then
	. ~/.bash/.bash_prompt
fi

clear
