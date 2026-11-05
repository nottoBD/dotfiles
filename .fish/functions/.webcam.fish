function .webcam --description 'C920 live view in this terminal (q or Ctrl-C to stop)'
    set -l cam (__c920); or return 1
    ffmpeg -hide_banner -loglevel fatal -nostats \
                -f v4l2 -input_format mjpeg -video_size 1920x1080 -framerate 30 -i $cam \
                -map 0:v -c:v copy -f nut pipe:1 |
        __c920_view webcam $argv
end
