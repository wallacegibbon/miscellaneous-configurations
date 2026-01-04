bindkey '^Q' quoted-insert
#bindkey -v
#stty -ixon

#function vi_escape() { zle vi-cmd-mode }
#zle -N vi_escape
#bindkey -M viins 'jf' vi_escape

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

alias ls="ls --color"


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
[[ ! -r '/home/wallace/.opam/opam-init/init.zsh' ]] || source '/home/wallace/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
# END opam configuration

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH=/usr/local/cuda/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/cuda/lib64:$LD_LIBRARY_PATH
