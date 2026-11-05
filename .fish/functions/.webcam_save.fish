function .webcam_save --description 'Record C920 1080p30 + mic to ~/media/videos/webcam, preview in this terminal (q or Ctrl-C to stop)'
    set -l cam (__c920); or return 1
    set -l dir $HOME/media/videos/webcam
    set -l out $dir/(date +%Y-%m-%d_%H-%M-%S).mkv
    mkdir -p $dir
    ffmpeg -hide_banner -loglevel fatal -stats \
                -f v4l2 -thread_queue_size 512 -ts mono2abs \
                -input_format mjpeg -video_size 1920x1080 -framerate 30 -i $cam \
                -f pulse -thread_queue_size 512 -i default \
                -map 0:v -map 1:a -c:v copy -c:a flac \
                -f tee "[f=matroska]$out|[f=nut:select=v:onfail=ignore]pipe:1" |
        __c920_view webcam_save $argv
end
