function .persopen --description 'Unlock personal gocryptfs folder'
    set -l doc ~/.local/share/Cryptomator/mnt/documents
    set -l mnt $XDG_RUNTIME_DIR/personal
    if not mountpoint -q $doc
        echo "documents vault is locked" >&2
        return 1
    end
    if mountpoint -q $mnt
        echo "already unlocked"
        return 0
    end
    mkdir -p $mnt && gocryptfs -i 15m $doc/personal.enc $mnt
end
