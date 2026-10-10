function gitfit --description 'stage files under 100 MiB, gitignore the rest'
    set -l limit 104857600
    for f in (git ls-files -m -o --exclude-standard)
        if not test -e $f
            git rm --cached -q -- $f
            continue
        end
        set -l s (stat -c %s -- $f)
        if test $s -lt $limit
            git add -- $f
        else
            echo "/$f" >> .gitignore
            echo "ignored "(math -s1 $s / 1048576)" MiB: $f"
        end
    end
    git add .gitignore
end
