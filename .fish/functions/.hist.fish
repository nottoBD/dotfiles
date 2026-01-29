function .hist --description 'grep through command history'
    if test (count $argv) -eq 0
        history
        return
    end
    history | command grep --color=auto -i -- $argv
end
