bindkey '^Q' quoted-insert
bindkey -v
stty -ixon

function vi_escape() {
	zle vi-cmd-mode  # Switch to normal mode
}
zle -N vi_escape
bindkey -M viins 'jf' vi_escape

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt append_history
setopt hist_ignore_dups
setopt share_history

case ":$PATH:" in
*:"$HOME/.local/bin":*)
	;;
*)
	export PATH="$HOME/.local/bin:$PATH"
	;;
esac
