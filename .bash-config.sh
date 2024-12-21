## Put this file in $HOME and add the following line to `$HOME/.bash_profile` or `$HOME/.bashrc`.
## . ~/.bash-config.sh

OS_MACHINE_TYPE=$(uname -sm | sed -n 's/^\([A-Za-z0-9_]*\).* \(.*\)/\1 \2/p')
PS1="\[\e[1;4;34m\]${OS_MACHINE_TYPE}\[\e[0m\][\[\e[33m\]\t\[\e[0m\]] \[\e[35m\]\w\[\e[0m\]\$ "

LOCAL_BIN=$HOME/.local/bin
case ":$PATH:" in
*:"$LOCAL_BIN":*) ;;
*               ) export PATH="$LOCAL_BIN:$PATH";;
esac
