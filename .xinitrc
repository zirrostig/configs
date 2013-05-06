#!/bin/bash

###Path
#echo 'Adding Stuff to Path' >/tmp/login_xinitrc
#Add xmonad to path
export PATH="/home/zirro/.cabal/bin:${PATH}"
#Add other stuff to path
export PATH="/home/zirro/perl5/bin:$PATH";
export PATH="/home/zirro/cfg/bin:$PATH";
export PATH="/home/zirro/app/bin:${PATH}";
export PATH="/home/zirro/cfg/bin:${PATH}";
export PATH="${PATH}:/home/zirro/.gem/ruby/1.9.1/bin:/home/zirro/.gem/ruby/1.8/bin"
#export PATH="${PATH}:/home/zirro/ap/android-sdk-linux/tools:/home/zirro/ap/android-sdk-linux/platform-tools";  #Android SDK

###More Path Additions
#Perl Crap
#echo 'Exporting Perl Shit' >>/tmp/login_xinitrc
export PERL_LOCAL_LIB_ROOT="/home/zirro/perl5";
export PERL_MB_OPT="--install_base /home/zirro/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/zirro/perl5";
export PERL5LIB="/home/zirro/perl5/lib/perl5/x86_64-linux-thread-multi:/home/zirro/perl5/lib/perl5";
#export PATH="/home/zirro/perl5/bin:$PATH";

#Enlightenment Settings
#echo 'Other exports' >>/tmp/login_xinitrc
export E_HOME=/home/zirro/.e/Desktop

#System Specific
#Move to if statement checking host
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=r600

#Wine Settings
export WINEARCH=win64

#echo 'Starting wallpaperShuffle' >>/tmp/login_xinitrc
wallpaperShuffle &

#xcompmgr -f -I .075 -O .075 -D 10 -c -r 20 -o .8 -C &
cairo-compmgr &

#echo 'Doing a bunch of stuff in /etc/X11/xinit/xinitrc.d' >>/tmp/login_xinitrc
#Load /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#echo 'Starting gpg-agent' >>/tmp/login_xinitrc
#eval $(gpg-agent --daemon) &

#echo 'Setting Keyboard Repeat' >>/tmp/login_xinitrc
xset r rate 300 25

#echo 'Loading Xresources' >>/tmp/login_xinitrc
xrdb -merge ~/.Xresources

#echo 'Starting xcape' >>/tmp/login_xinitrc
#xcape &

#echo 'Starting dropbox' >>/tmp/login_xinitrc
dropboxd start &

[[ "$1" -eq 'enlightenment_start' ]] && xrdb -merge <<< 'URxvt.transparent: false' || echo 'Setting urxvt.transparent to true' >>/tmp/login_xinitrc

#echo "Starting $1" >>/tmp/login_xinitrc
exec $1
