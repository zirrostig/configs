#!/bin/bash

xrdb -merge ~/.Xresources

/home/zirro/cfg/bin/wallpaperShuffle &

xcompmgr -f -I .075 -O .075 -D 10 -c -r 20 -o .8 -C &

#Load /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#Add xmonad to path
export PATH="/home/zirro/.cabal/bin:${PATH}"

exec $1


