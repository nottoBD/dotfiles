

if status is-interactive
    # Commands to run in interactive sessions can go here
end



# quiet startup
set -g fish_greeting
set -x CHEAT_PATH "$HOME/.config/cheat/cheatsheets/personal:$HOME/.config/cheat/cheatsheets/community"
# prefer vi keybindings
# fish_vi_key_bindings
starship init fish | source
