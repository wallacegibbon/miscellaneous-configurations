## Put this file in $HOME and add the following line to `$HOME/.bash_profile` or `$HOME/.bashrc`.
## . ~/.bash-config.sh

PS1="\[\e[1;4;34m\]$(uname -sm | sed -n 's/^\([A-Za-z0-9_]*\).* \(.*\)/\1 \2/p')\[\e[0m\][\[\e[33m\]\t\[\e[0m\]] \[\e[35m\]\w\[\e[0m\]\$ "

