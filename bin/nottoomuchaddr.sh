#!/bin/sh

#Got from
#http://blog.tshirtman.fr/2013/3/17/mutt-offlineimap-notmuch-nottoomuch

/usr/bin/nottoomuch-addresses.sh "$1" \
    | sed -s 's/\(.*\) \(<.*\)/\2\   \1/'\
    | sed -s 's/\"//g'\
    | sed -s '/buzz+.*/d'\
