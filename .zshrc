################################################################################
### All Knowing, All Useful Website
###  http://zsh.sourceforge.net/Doc/
################################################################################

################################################################################
### Misc Options
################################################################################
setopt DVORAK       #Dvorak keymap is used for smart spelling correction
setopt C_BASES      #Output hex as 0x... rather than 16#...
setopt C_PRECEDENCES #Change order of opts to be more c like
unsetopt BEEP       #No Annoying beep

################################################################################
### Directory Options
################################################################################
setopt AUTO_CD      #cd not necessary anymore

################################################################################
### Completion Options, Expansion, Globbing
################################################################################
setopt AUTO_LIST            #Shows choices on ambiguous
setopt LIST_AMBIGUOUS       #Fills to ambiguous, then lets AUTO_LIST work
setopt LIST_TYPES
setopt BAD_PATTERN
setopt CASE_MATCH
setopt NOMATCH
setopt NUMERIC_GLOB_SORT
setopt REMATCH_PCRE         #Perl-Style Regex :)
setopt EXTENDEDGLOB
autoload -U compinit && compinit

################################################################################
### History Settings
################################################################################
HISTFILE=$HOME/.zsh_hist
HISTSIZE=65536
SAVEHIST=65536
setopt HIST_IGNORE_DUPS     #if command matches previous, don't write to history
setopt HIST_IGNORE_SPACE    #if command begins with space don't write to history
setopt HIST_REDUCE_BLANKS
setopt APPEND_HISTORY       #Good for running multiple zsh sessions
                            # simultaneously
setopt BANG_HIST

################################################################################
### Zsh Specials
################################################################################
autoload -U zmv
autoload -U tetris && zle -N tetris   #Because we can

################################################################################
### Zsh Modules
################################################################################
zmodload zsh/pcre       #pcre_compile and pcre_match
zmodload zsh/datetime
zmodload zsh/mathfunc

################################################################################
### Keyboard Input Settings
################################################################################
#bindkey -v                                      #Vim keybindings
#Fix some keys to work as I expect them to
# Keybindings
bindkey '[3~' delete-char            #Forward Delete Key
bindkey '^r' history-incremental-search-backward   #^r now works - history search
bindkey '^[l' push-line

################################################################################
### Aliases
################################################################################
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -la'
alias allcolors='(x=`tput op` y=`printf %80s`;for i in {0..255};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)'
alias killFlash='~/.killFlash.sh'
alias connectToSimon='ssh -Nfq -L 2676:simon.mines.edu:22 zstigall@imagine.mines.edu'
alias makeJavaWindowsWork='wmname LG3D'
alias vim=gvim

################################################################################
### Functions
################################################################################
#Executed before the prompt is displayed
function precmd {
    vcs_info
    setprompt
    venv_rprompt

    #Change urxvt's title to be the last command and current directory to 2 dirs
    if [[ "$TERM" == "rxvt-unicode-256color" ]]; then
        local dir=${(%):-%2/}
        printf '\33]2;%s\007' "% $my_lastcmd : $dir"
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

function cd {
if (( $# == 0 )) && [[ `pwd` != $HOME ]]
    then builtin pushd -q ~/
    else builtin cd $*
    fi
}

function rationalise-dot {
    if [[ $LBUFFER = *..  ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}

zle -N rationalise-dot
bindkey . rationalise-dot

################################################################################
### Various Exports
################################################################################
export EDITOR="vim";
export BROWSER="firefox";
export WINEARCH="win32";

################################################################################
### CSCI410 Tecs HW Sim
################################################################################
function tecshw {
    /home/zirro/csm/f12/csci410/tecs/HardwareSimulator.sh $PWD/$1
}
function tecsasm {
    if [ $# -ge 1 ]; then
        /home/zirro/csm/f12/csci410/tecs/Assembler.sh $PWD/$1 $PWD/$2
    else
        /home/zirro/csm/f12/csci410/tecs/Assembler.sh $PWD/$1
    fi
}

################################################################################
### My Prompt
################################################################################
#Settings
setopt PROMPT_SUBST             #Allows variable expansion in the prompt string
setopt PROMPT_BANG              #Replace ! (bang) with history event number
setopt PROMPT_PERCENT           #% treated special
setopt TRANSIENT_RPROMPT        #Used when accepting commands from copy/paste 
                                # or otherwise

#Load some stuff up
autoload -Uz vcs_info
#autoload -U promptinit
autoload -U colors && colors

#Easier Color Usage
local reset white gray green red
reset="%{${reset_color}%}"
white="%{$fg[white]%}"
green="%{$fg[green]%}"
blue="%{$fg[blue]%}"
red="%{$fg[red]%}"
yellow="%{$fg[yellow]%}"

# Set up VCS_INFO
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' get-revision true
zstyle ':vcs_info:git:*' check-for-changes true

zstyle ':vcs_info:git*' formats "(%s) %12.12i %c%u %b%m" # hash changes branch misc
zstyle ':vcs_info:git*' actionformats "(%s|${white}%a${gray}) %12.12i %c%u %b%m"

zstyle ':vcs_info:git*:*' stagedstr "${green}S${gray}"
zstyle ':vcs_info:git*:*' unstagedstr "${red}U${gray}"

zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash git-untracked

#Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
    local ahead behind remote
    local -a gitstatus

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name --abbrev-ref 2>/dev/null)}

    if [[ -n ${remote} ]] ; then
        # for git prior to 1.7
        # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
        ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
        (( $ahead )) && gitstatus+=( "${green}+${ahead}${gray}" )

        # for git prior to 1.7
        # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
        behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
        (( $behind )) && gitstatus+=( "${red}-${behind}${gray}" )

        hook_com[branch]="${hook_com[branch]} [${remote} ${(j:/:)gitstatus}]"
    fi
}

# Show count of stashed changes
function +vi-git-stash() {
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        hook_com[misc]+=" (${stashes} stashed)"
    fi
}

# Indicate if there are any untracked files present
function +vi-git-untracked() {
    local untracked

    #check if there's at least 1 untracked file
    untracked=${$(git ls-files --exclude-standard --others | head -n 1)}

    if [[ -n ${untracked} ]] ; then
        hook_com[unstaged]="${hook_com[unstaged]}${yellow}?${gray}"
    fi
}

function setprompt() {
    local -a lines infoline
    local x i pet dungeon filler i_width i_pad

    #My pet
    pet=@

    #Current Directory
    if [[ -w $PWD ]] ;
        then infoline+=${green} || infoline+=${yellow}
        else infoline+="%4/${reset}"
    fi

    #Username & Host
    infoline+="%n"
    infoline+="%m"

    zstyle -T ":pr-nethack:" show-pet && i_pad=4 || i_pad=0

    # Strip color to find text width & make the full-width filler
    i_width=${(S)infoline//\%\{*?\%\}} # search-and-replace color escapes
    i_width=${#${(%)i_width}} # expand all escapes and count the chars


    filler="${blue}${(l:$(( $COLUMNS - $i_width - $i_pad ))::.:)}${reset}"
    infoline[2]=( "${infoline[2]} ${filler} " )

     ### Now, assemble all prompt lines
    lines+=( ${(j::)infoline} )
    [[ -n ${vcs_info_msg_0_} ]] && lines+="${gray}${vcs_info_msg_0_}${reset}"
    lines+="%(1j.${blue}%j${reset} .)%(0?.${white}.${red})%#${reset} "

    ### Add dungeon floor to each line
    # Allow easy toggling of pet display:
    # zstyle ':pr-nethack:' show-pet false
    if zstyle -T ":pr-nethack:" show-pet ; then
        dungeon=${(l:$(( ${#lines} * 3 ))::.:)}
        dungeon[$[${RANDOM}%${#dungeon}]+1]=$pet

        for (( i=1; i < $(( ${#lines} + 1 )); i++ )) ; do
            case $i in
                1) x=1;; 2) x=4;; 3) x=7;; 4) x=10;;
            esac
            lines[$i]="${red}${dungeon[x,$(( $x + 2 ))]} ${lines[$i]}${reset}"
        done
    fi

    PROMPT=${(F)lines}
}

venv_rprompt() {

}

##Prompt generation
#local -a dirString
##Directory name color, green for writable, yellow for not
#[[ -w $PWD ]] && dirString += ( ${green} ) || dirString += ( ${yellow} )
#dirString += ( "%4/ " )
#dirString += ( "${reset}" )
#builtin print -p dirString

#The Prompts
#PS1="%F{red}%M%f %F{green}%4/%f %(?.%?%B%#%b.%F{red}%?%B%b%f%#) "
##Displays: *** History# Hostname CurDir(4 max) ReturnStatus PromptThing(%or#)
#RPROMPT="%F{yellow}%*%f"                 #Displays: *** HH:mm:ss (%*)
PS2="%_ > "
PS4="%_ %i>> "
