xrdb -merge ~/.Xresources
# xmodmap ~/.Xmodmap
# xcape -e 'Shift_L=Escape'
# ibus-daemon -xd
# xrandr --output HDMI-1 --mode 1280x720 --rotate left
xrandr --output VGA-1 --mode 1920x1080
xset b off s off -dpms
uxterm -geometry 80x24+100+20 &
exec icewm
