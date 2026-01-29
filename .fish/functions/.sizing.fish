function .sizing --description 'Top-level disk usage (big dirs/files)'
    sudo du -h -x / \
    | grep -E '[0-9\.]+G|[5-9][0-9][0-9]M' \
    | sort -rh
end
