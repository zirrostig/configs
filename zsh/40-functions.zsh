# TO BE SOURCED FROM $HOME/.zshrc

###############################################################################
# Functions
###############################################################################

#Executed before the prompt is displayed
function precmd {
    vcs_info
    setprompt
    venv_rprompt

    if [[ "$TERM" == "rxvt-unicode-256color" ]]; then
        printf '\33]2;%% %s\007' "$PWD"
    fi
}

#Executed before the command is executed, after the enter key is pressed
function preexec {
    #Change urxvt's title to the (soon to be) running command
    #   Look at adding truncation (for very long commands) to this later
    if [[ "$TERM" == "rxvt-unicode-256color" ]]; then
        #my_lastcmd=${1[(wr)^(*=*|sudo|-*)]}
        my_lastcmd=$1
        printf '\33]2;%s\007' "- $my_lastcmd"
    fi
}

#Executed whenever I change directories
function chpwd {
    #Change urxvt's title to be the current directory to 2 dirs
    if [[ "$TERM" == "rxvt-unicode-256color" ]]; then
        local dir=${(%):-%2/}
        printf '\33]2;%s\007' "% $dir"
    fi
}

# cdr has replaced this
# function cd {
# if (( $# == 0 )) && [[ `pwd` != $HOME ]]
#     then builtin pushd -q ~/
#     else builtin cd $*
#     fi
# }


function reload {
    source $HOME/.zshrc
}

function vboxRawDisk {
    if [[ $# == 0 ]] || [[ $1 == '-h' ]] || [[ $1 == '--help' ]]
    then echo 'USAGE: vboxRawDisk vmdkFileName rawDisk'
    else sudo VBoxManage internalcommands createrawvmdk -filename $1 -rawdisk $2
    fi
}

function power-info {
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -A2 energy-rate
}

################################################################################
### Widgets and whatnot
################################################################################

#Replaces ... -> ../.. on the fly
function rationalise-dot {
    if [[ $LBUFFER = *..  ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
bindkey -s '`' '~/'

#Function to set our prompt to show the current mode
function zle-line-init zle-keymap-select {
    setprompt #Updated to add mode indicator
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

#Fuzzy Files
FUZZIGNOREDIRS=('_darcs' '.darcs' '.git' 'dist' '.cabal' '.local' '.sparkleshare' '.vim/bundle' '.config' '.neocomplete' '.mozilla' '.vimlocal' '.crawl' '.cache')
FUZZIGNOREEXTS=('hi' 'o' 'aux')
zle -C fuzzy menu-expand-or-complete fuzzy-files
bindkey '^[t' fuzzy
function fuzzy-files {
    if [[ $PREFIX == */* ]]; then
        fuzzdir=${PREFIX%/*}
    else
        fuzzdir=.
    fi

    prunedirs=$(
        for d in $FUZZIGNOREDIRS; do
            echo -n " -path '*/${d}' -prune -o"
        done
    )
    ignoreexts=$(
        for ext in $FUZZIGNOREEXTS; do
            echo -n "-a ! -iname '*.${ext}' "
        done
    )

    fuzzpat=${PREFIX##*/}
    # echo "\nfind $fuzzdir $prunedirs -type f $ignoreexts"
    compadd -U $(eval "find ${fuzzdir} ${prunedirs} -type f ${ignoreexts}" | matcher "${fuzzpat}")
}


################################################################################
### CSCI410 Tecs HW Sim
################################################################################
function tecshw {
    /usr/local/tecs/HardwareSimulator.sh $PWD/$1
}
function tecsasm {
    if [ $# -ge 1 ]; then
        /usr/local/tecs/Assembler.sh $PWD/$1 $PWD/$2
    else
        /usr/local/tecs/Assembler.sh $PWD/$1
    fi
}
function tecsc {
    /usr/local/tecs/JackCompiler.sh $PWD/$1
}
function tecsvm {
    /usr/local/tecs/VMEmulator.sh $PWD/$1
}
function tecscpu {
    /usr/local/tecs/CPUEmulator.sh $PWD/$1
}

