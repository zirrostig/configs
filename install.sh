#!/usr/bin/zsh

exe_name=${0##*/}
exe_path=${0%/*}
cfg_dir=${PWD##*/}

#Make sure we are running in the same directory as this script
if [[ ! -f ./$exe_name ]];then
    echo "Please 'cd $exe_path' then run this script"
    exit 1
fi

cd $HOME

ln -sv "$cfg_dir/Xresources" "./.Xresources"
ln -sv "$cfg_dir/crawlrc" "./.crawlrc"
ln -sv "$cfg_dir/ghci" "./.ghci"
ln -sv "$cfg_dir/hgrc" "./.hgrc"
ln -sv "$cfg_dir/notmuch-config" "./.notmuch-config"
ln -sv "$cfg_dir/sqliterc" "./.sqliterc"
ln -sv "$cfg_dir/xmobarrc" "./.xmobarrc"
ln -sv "$cfg_dir/offlineimap/offlineimaprc" "./.offlineimaprc"

#These have directories
mkdir -p "./.mutt"
for mf in "$cfg_dir/mutt/*"; do
    ln -sv "$mf" "./.mutt/${mf##*/}"
done

mkdir -p "./xmonad/"
cp "$cfg_dir/xmonad.hs" "./.xmonad/xmonad.hs"


echo "Setting ZDOTDIR in /etc/profile. need sudo"
echo 'export ZDOTDIR=$HOME/.zsh' | sudo tee -a /etc/profile
mkdir -p "./.zsh"
ln -sv "$cfg_dir/zshrc" "./.zsh/.zshrc"

cd -
