# ~/.config/fish/functions/import_bash_exports.fish
function import_bash_exports --description 'Convert simple Bash exports to fish env'
    set -l src "$HOME/.bash/exports"
    test -f $src; or begin
        echo "No $src found."; return 1
    end

    set -l dst "$HOME/.config/fish/conf.d/01-imported-env.fish"
    echo "# auto-generated from ~/.bash/exports on (date)" | sed "s/(date)/"(date)"/" > $dst

    # very simple parser: export NAME=val or NAME=val ; handles quoted vals
    while read -l line
        set line (string trim -- $line)
        test -z "$line"; and continue
        string match -rq '^(#|;)' -- $line; and continue

        if string match -rq '^export ' -- $line
            set line (string replace -r '^export[[:space:]]+' '' -- $line)
        end

        if string match -rq '^[A-Za-z_][A-Za-z0-9_]*=' -- $line
            set -l name (string replace -r '=.*$' '' -- $line)
            set -l val (string replace -r '^[^=]+=' '' -- $line)
            # strip surrounding quotes if present
            set val (string replace -r '^"(.*)"$' '$1' -- $val)
            set val (string replace -r "^'(.*)'\$" '$1' -- $val)
            printf 'set -gx %s %s\n' $name (string escape -- $val) >> $dst
        end
    end < $src

    echo "Wrote $dst"
    source $dst
end

