## Copy this file to $HOME. In `$HOME/.bash_profile` or `$HOME/.bashrc`, add the following command:
## . ~/.bash-config.sh

OS_MACHINE_TYPE=$(uname -sm | sed -n 's/^\([A-Za-z0-9_]*\).* \(.*\)/\1 \2/p')
PS1="\[\e[1;4;34m\]${OS_MACHINE_TYPE}\[\e[0m\][\[\e[33m\]\t\[\e[0m\]] \[\e[35m\]\w\[\e[0m\]\$ "
