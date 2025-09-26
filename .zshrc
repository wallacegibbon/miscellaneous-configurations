bindkey -v
bindkey '^Q' quoted-insert
stty -ixon

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
