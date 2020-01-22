xrdb ~/.Xresources
# xmodmap ~/.Xmodmap
# xcape -e 'Shift_L=Escape'
# xset b off
# xset s off
# xset -dpms
# ibus-daemon -xd
# xrandr --output DP1 --rotate left
# xrandr --output VGA-1 --mode 1920x1080
xrandr --output HDMI-1 --mode 1280x720
xterm -geometry 80x40+200+0 &
exec openbox
