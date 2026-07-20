# POSIX compliant

# ---------------------------------------------------------------------------
# XDG base directories
# ---------------------------------------------------------------------------
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_USER_DIRS_DISABLE=1

# elogind/PAM normally set at login
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export XDG_SESSION_TYPE=x11

# ---------------------------------------------------------------------------
# Core
# ---------------------------------------------------------------------------
export EDITOR=nvim
export VISUAL=nvim
export PAGER=less
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
export HISTCONTROL=ignorespace
export GO111MODULE=on
export WINEARCH=win32
export GPG_ID=F12382BBA5604F9C1781BA21332971AAE7B0FDB9
export GPT_TTY="$(tty)"
export LD_LIBRARY_PATH="/opt/cuda/lib64:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/lib/pkgconfig:/usr/local/lib/pkgconfig"

# ---------------------------------------------------------------------------
# History / state redirects
# ---------------------------------------------------------------------------
export HISTFILE="$XDG_STATE_HOME/bash/history"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export LESSHISTFILE="$XDG_STATE_HOME/less/history"
export PYTHONHISTFILE="$XDG_STATE_HOME/python/history"
export PYTHONPYCACHEPREFIX="$XDG_CACHE_HOME/python"

# ---------------------------------------------------------------------------
# Tool config dirs
# ---------------------------------------------------------------------------
export CHEAT_PATH="$HOME/.config/cheat/cheatsheets/personal:$HOME/.config/cheat/cheatsheets/community"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export PASSWORD_STORE_DIR="$HOME/.password-store"
export TEXMFVAR="$XDG_CACHE_HOME/texlive/texmf-var"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export VIMDATA="$XDG_DATA_HOME/nvim"
export GHCUP_USE_XDG_DIRS=1

# ---------------------------------------------------------------------------
# X / XMonad
# ---------------------------------------------------------------------------
export XMOCONF="$HOME/.xmonad/xmonad.hs"
export XMONAD_CONFIG_DIR="$HOME/.xmonad"
export XMONAD_DATA_DIR="$HOME/.local/share/xmonad"
export XMONAD_CACHE_DIR="$HOME/.cache/xmonad"
# NOTE: startx/xinit read ~/.xinitrc directly and ignore $XINITRC
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"

# ---------------------------------------------------------------------------
# Audio runtime dirs
# ---------------------------------------------------------------------------
export PIPEWIRE_RUNTIME_DIR="/run/user/$(id -u)"
export PULSE_RUNTIME_DIR="/run/user/$(id -u)"

# ---------------------------------------------------------------------------
# PATH — single authoritative definition.
# ---------------------------------------------------------------------------
export PATH="$HOME/.cargo/bin:$HOME/.config/emacs/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/local/go/bin:$HOME/go/bin:$HOME/.local/bin:$HOME/.ghcup/bin:$HOME/.spicetify"
