function .persolock --description 'Lock personal gocryptfs folder'
    set -l mnt $XDG_RUNTIME_DIR/personal
    if not mountpoint -q $mnt
        echo "already locked"
        return 0
    end
    fusermount3 -u $mnt
end
