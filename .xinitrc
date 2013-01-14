#!/bin/bash

#Add xmonad to path
export PATH="/home/zirro/.cabal/bin:${PATH}"
#Add other stuff to path
export PATH="/home/zirro/perl5/bin:$PATH";
export PATH="/home/zirro/cfg/bin:$PATH";
export PATH="/home/zirro/app/bin:${PATH}";
export PATH="/home/zirro/cfg/bin:${PATH}";
export PATH="${PATH}:/home/zirro/.gem/ruby/1.9.1/bin:/home/zirro/.gem/ruby/1.8/bin"
#export PATH="${PATH}:/home/zirro/ap/android-sdk-linux/tools:/home/zirro/ap/android-sdk-linux/platform-tools";  #Android SDK

###Path Additions
#Perl Crap
export PERL_LOCAL_LIB_ROOT="/home/zirro/perl5";
export PERL_MB_OPT="--install_base /home/zirro/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/zirro/perl5";
export PERL5LIB="/home/zirro/perl5/lib/perl5/x86_64-linux-thread-multi:/home/zirro/perl5/lib/perl5";
#export PATH="/home/zirro/perl5/bin:$PATH";

#Wine Settings
export WINEARCH=win64

wallpaperShuffle &

#xcompmgr -f -I .075 -O .075 -D 10 -c -r 20 -o .8 -C &

#Load /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xset r rate 300 25
xrdb -merge ~/.Xresources
xcape &

dropboxd start &

exec $1
