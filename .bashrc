# PS1='\[\033[37;44m\]\u@\h\[\033[00m\] \[\033[34m\]\w\n$\[\033[00m\] '
# PS1='\[\033[4m\]\u@\h\[\033[0m\] \w\n$ '
# PS1='\u@\h \w\n$ '
PS1='\u:\w\$ '

## for Mac, you can use special characters like:
PS1="\h:\W \u ➦ "

# setterm -blength 0  # no need if /etc/inputrc already disabled bell

# if you can not add "set editing-mode vi" to /etc/inputrc, just:
set -o vi

bind '"\C-i": complete'
bind '"\C-l": clear-screen'
mesg n

# for clojure
alias clojure="rlwrap -r -c -b \"(){}[],^%\\\$\#@\\\"\\\";:''|\\\\\" java -cp \`cat ~/leinpath\` clojure.main"

alias cljs="rlwrap -r -c -b \"(){}[],^%\\\$\#@\\\"\\\";:''|\\\\\" java -cp \`cat ~/leinpath\` clojure.main -e \"(require 'cljs.repl.node 'cljs.repl)(cljs.repl/repl (cljs.repl.node/repl-env))\""

