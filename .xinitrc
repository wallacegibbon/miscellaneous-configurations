xrdb -merge ~/.Xresources
# xmodmap ~/.Xmodmap
# xcape -e 'Shift_L=Escape'
# xset b off s off -dpms
# ibus-daemon -xd
# xrandr --output HDMI-1 --mode 1280x720 --rotate left
uxterm -geometry 80x24+100+20 &
xrandr --output VGA-1 --mode 1920x1080
exec icewm
