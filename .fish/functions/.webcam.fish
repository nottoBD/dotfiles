function .webcam --description 'C920 live view'
    set -l cam /dev/v4l/by-id/usb-046d_HD_Pro_Webcam_C920-video-index0
    v4l2-ctl -d $cam -c exposure_dynamic_framerate=0 2>/dev/null
    or v4l2-ctl -d $cam -c exposure_auto_priority=0 2>/dev/null
    mpv --profile=webcam av://v4l2:$cam $argv
end
