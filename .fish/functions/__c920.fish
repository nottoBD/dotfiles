function __c920 --description 'Print the C920 capture node; wait up to 5 s for udev after boot/replug'
    set -l cam /dev/v4l/by-id/usb-046d_HD_Pro_Webcam_C920-video-index0
    for i in (seq 50)
        test -c $cam; and break
        sleep 0.1
    end
    if not test -c $cam
        echo 'C920: not connected' >&2
        return 1
    end
    v4l2-ctl -d $cam -c exposure_dynamic_framerate=0 &>/dev/null
    or v4l2-ctl -d $cam -c exposure_auto_priority=0 &>/dev/null
    echo $cam
end
