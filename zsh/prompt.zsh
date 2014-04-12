# TO BE SOURCED FROM $HOME/.zshrc

################################################################################
# My Prompt
# A lot of this came from Seth House's Prompt found here \/
#  https://github.com/whiteinge/dotfiles/blob/master/.zsh_shouse_prompt
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
local reset white gray green red cyan blue yellow
reset="%{${reset_color}%}"
white="%{$fg[white]%}"
gray="%{$fg[black]%}"
green="%{$fg[green]%}"
blue="%{$fg[blue]%}"
cyan="%{$fg[cyan]%}"
red="%{$fg[red]%}"
yellow="%{$fg[yellow]%}"

# Set up VCS_INFO
zstyle ':vcs_info:*' enable git hg bzr darcs
zstyle ':vcs_info:*:*' check-for-changes true
zstyle ':vcs_info:*:*' get-revision true

zstyle ':vcs_info:*' formats "(%s) %12.12i %c%u %b%m" # hash changes branch misc
zstyle ':vcs_info:*' actionformats "(%s|${white}%a${red}) %12.12i %c%u %b%m"

zstyle ':vcs_info:*:*' stagedstr "${green}S${blue}"
zstyle ':vcs_info:*:*' unstagedstr "${red}U${yellow}"

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
        (( $ahead )) && gitstatus+=( "${green}+${ahead}${red}" )

        # for git prior to 1.7
        # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
        behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
        (( $behind )) && gitstatus+=( "${red}-${behind}${red}" )

        hook_com[branch]="${hook_com[branch]} [${remote}${(j:/:)gitstatus}]"
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
        hook_com[unstaged]="${hook_com[unstaged]}${yellow}?${red}"
    fi
}

function setprompt() {
    local -a lines infoline
    local  i filler i_width i_pad vimd

    #My pet and dungeon settings
    local x pet dungeon dungeon_width
    pet=@
    dungeon_width=4

    #Username
    infoline+="${blue}%n${reset} "

    #Host
    if [[ -n "$SSH_CLIENT" ]]; then
        infoline+="${cyan}%m${reset} "
    fi

    #Current Directory
    if [[ -w $PWD ]]; then
        infoline+=${green}
    else
        infoline+=${yellow}
    fi
    infoline+="%4/${reset}"     #Current Directory (out to 4 directories)- $4/

    #VimMode
    case $KEYMAP in
        vicmd) print -rn -- $terminfo[civis];
               vimd='[N]';;
            *) print -rn -- $terminfo[cnorm];
               vimd='[I]';;
    esac

    #This is awesome
    #Makes dungeon
    zstyle -T ":pr-nethack:" show-pet && i_pad=$(( $dungeon_width+1 )) || i_pad=0

    ### Test Colors
    #lines+="${white}white${reset} ${gray}gray${reset} ${green}green${reset} ${blue}blue${reset} ${cyan}cyan${reset} ${red}red${reset} ${yellow}yellow${reset}"

    ### Now, assemble all prompt lines
    lines+=( ${(j::)infoline} )
    [[ -n ${vcs_info_msg_0_} ]] && lines+="${yellow}${vcs_info_msg_0_}${reset}"
    lines+="$vimd %(1j.${blue}%j${reset} .)%(0?.${white}.${red})%? %#${reset} "

    ### Add dungeon floor to each line
    # Allow easy toggling of pet display:
    # zstyle ':pr-nethack:' show-pet false
    if zstyle -T ":pr-nethack:" show-pet ; then
        dungeon=${(l:$(( ${#lines} * $dungeon_width ))::.:)}
        dungeon[$[${RANDOM}%${#dungeon}]+1]=$pet

        for (( i=1; i < $(( ${#lines} + 1 )); i++ )) ; do
            x=$(( 1 + ( $i - 1 ) * $dungeon_width ))
            lines[$i]="${red}${dungeon[x,$(( $x + $dungeon_width - 1 ))]} ${lines[$i]}${reset}"
        done
    fi

    #Joins prompt lines together with \n (F)
    PROMPT=${(F)lines}
}

venv_rprompt() {
    local line nextdir

    #Get battery level
    local bat bat_full bat_pcent
    typeset -F bat bat_full bat_pcent
    bat_full=$(cat /sys/class/power_supply/BAT0/charge_full_design)
    bat=$(cat /sys/class/power_supply/BAT0/charge_now)
    bat_pcent=$(( ($bat / $bat_full) * 100 ))

    line=$(printf "%0.0f%%%%" $bat_pcent)

    # #Add first element on the directory stack to the prompt
    # #Get the next element on the directory stack
    # nextdir=${(w)dirstack[1]}
    # nextdir=${nextdir/$HOME/\~}         #Replace /home/user with '~'
    # lines+="${cyan}${nextdir}${reset}"

    # #If we're in a virtual environment, make it known
    # if [[ -n $VIRTUAL_ENV ]]; then
    #     lines+="${red}venv:$(basename $VIRTUAL_ENV)${reset}"
    # fi

    RPROMPT=$line
}

#(R)Prompt is set in precmd()

#Make nice descending prompt for if/while/for/case statements
PS2="%_ > "
PS4="%_ %i>> "
