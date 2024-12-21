## Put this file in $HOME and add the following line to `$HOME/.bash_profile` or `$HOME/.bashrc`.
## . ~/.bash-config.sh

OS_MACHINE_TYPE=$(uname -sm | sed -n 's/^\([A-Za-z0-9_]*\).* \(.*\)/\1 \2/p')
PS1="\[\e[1;4;34m\]${OS_MACHINE_TYPE}\[\e[0m\][\[\e[33m\]\t\[\e[0m\]] \[\e[35m\]\w\[\e[0m\]\$ "

ELIXIR_HOME="$HOME/elixir-1.18.0-otp-26"

case ":$PATH:" in
  *:"$ELIXIR_HOME/bin":*) ;;
  *) export PATH="$ELIXIR_HOME/bin:$PATH";;
esac
