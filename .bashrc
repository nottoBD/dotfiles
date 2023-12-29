# /etc/bash.bashrc
[[ $- != *i* ]] && return


if [ -f ~/.bash_exports ]; then
    . ~/.bash_exports
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.bash_prompt ]; then
    . ~/.bash_prompt
fi

clear
