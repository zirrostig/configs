#!/bin/bash

#Add xmonad to path
export PATH="/home/zirro/.cabal/bin:${PATH}"
#Add other stuff to path
export PATH="/home/zirro/perl5/bin:$PATH";
export PATH="/home/zirro/.cabal/bin:$PATH";
export PATH="/home/zirro/cfg/bin:$PATH";

wallpaperShuffle &

xcompmgr -f -I .075 -O .075 -D 10 -c -r 20 -o .8 -C &

#Load /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xset r rate 300 25
xrdb -merge ~/.Xresources

dropboxd start &

exec $1


