#!/bin/bash
if [ "$TERM" = "linux" ]; then
    echo -en "\e]P0181A1E" #black
    echo -en "\e]P8474B51" #darkgray
    echo -en "\e]P1A54242" #darkred
    echo -en "\e]P9CC6666" #red
    echo -en "\e]P27C8440" #darkgreen
    echo -en "\e]PAB5BD68" #green
    echo -en "\e]P3DE935F" #orange
    echo -en "\e]PBF0C674" #yellow
    echo -en "\e]P43F557D" #darkblue
    echo -en "\e]PC81A2BE" #blue
    echo -en "\e]P585678F" #magenta
    echo -en "\e]PDB294BB" #purple
    echo -en "\e]P65E8D87" #cyan
    echo -en "\e]PE8ABED8" #aqua
    echo -en "\e]P7707880" #light-gray
    echo -en "\e]PFBFBFBF" #white
    clear #Removes artefacts
fi
