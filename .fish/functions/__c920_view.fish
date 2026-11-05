function __c920_view --description 'mpv sink for a NUT stream on stdin, drawn inside the calling terminal'
    set -l title $argv[1]
    set -e argv[1]
    set -l embed
    set -q WINDOWID; and set embed --wid=$WINDOWID
    mpv --profile=low-latency --untimed --no-cache --keep-open=no \
                --terminal=no --input-vo-keyboard=no --input-cursor=no \
                --title=$title $embed $argv -
end
