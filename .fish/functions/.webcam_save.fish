function .webcam_save --description 'Record C920 1080p30 + mic to ~/media/videos/webcam'
    set -l cam /dev/v4l/by-id/usb-046d_HD_Pro_Webcam_C920-video-index0
    set -l dir $HOME/media/videos/webcam
    mkdir -p $dir
    v4l2-ctl -d $cam -c exposure_dynamic_framerate=0 2>/dev/null
    or v4l2-ctl -d $cam -c exposure_auto_priority=0 2>/dev/null
    ffmpeg -hide_banner \
                -f v4l2 -thread_queue_size 512 -ts mono2abs \
                -input_format mjpeg -video_size 1920x1080 -framerate 30 -i $cam \
                -f pulse -thread_queue_size 512 -i default \
                -map 0:v -map 1:a -c:v copy -c:a flac \
                $dir/(date +%Y-%m-%d_%H-%M-%S).mkv
end
