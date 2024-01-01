cd_func() {
    if [ "$#" -eq 0 ]; then
        builtin cd ~ && exa -al --color=always --group-directories-first
    else
        builtin cd "$@" && [ "$(ls -1 | wc -l)" -lt 25 ] && exa -al --color=always --group-directories-first
    fi
}
