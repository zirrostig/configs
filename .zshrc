###
#All Knowing, All Useful Website
# http://zsh.sourceforge.net/Doc/
###

### My Prompt
#Settings
setopt PROMPT_SUBST     #Expansion stuff
setopt PROMPT_BANG      #Replace ! (bang) with history event number
setopt PROMPT_PERCENT   #% treated special
setopt TRANSIENT_RPROMPT    #Used when accepting commands from copy/paste or otherwise
#The Prompts
PS1="%F{cyan}%M%f %F{green}%4/%f %(?.%?%B%#%b.%F{red}%?%B%b%f%#) "    #Displays: *** History# Hostname CurDir(4 max) ReturnStatus PromptThing(%or#)
RPROMPT="%F{cyan}%*%f"                 #Displays: *** HH:mm:ss (%*)
PS2="%_ > "
PS4="%_ %i>> "

###Misc Options
setopt DVORAK       #Dvorak keymap is used for smart spelling correction
setopt C_BASES      #Output hex as 0x... rather than 16#...
setopt C_PRECEDENCES #Change order of opts to be more c like
unsetopt BEEP       #No Annoying beep

###Directory Options
setopt AUTO_CD      #cd not necessary anymore

###Completion Options, Expansion, Globbing
setopt AUTO_LIST            #Shows choices on ambiguous
setopt LIST_AMBIGUOUS       #Fills to ambiguous, then lets AUTO_LIST work
setopt LIST_TYPES
setopt BAD_PATTERN
setopt CASE_MATCH
setopt NOMATCH
setopt NUMERIC_GLOB_SORT
setopt REMATCH_PCRE         #Perl-Style Regex :)

###History Settings
HISTFILE=$HOME/.zsh_hist
HISTSIZE=65536
SAVEHIST=65536
setopt HIST_IGNORE_DUPS     #if command matches previous, don't write to history
setopt HIST_IGNORE_SPACE    #if command begins with space don't write to history
setopt HIST_REDUCE_BLANKS
setopt APPEND_HISTORY       #Good for running multiple zsh sessions simultaneously
setopt BANG_HIST

###Zsh Specials
autoload -U zmv

### Keyboard Input Settings
#bindkey -v                                      #Vim keybindings
#Fix some keys to work as I expect them to
# Keybindings
bindkey '[3~' delete-char            #Forward Delete Key
bindkey '^r' history-incremental-search-backward   #^r now works - history search
bindkey '^[l' push-line

###Aliases
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -la'
alias allcolors='(x=`tput op` y=`printf %80s`;for i in {0..255};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)'
alias killFlash='~/.killFlash.sh'
alias connectToSimon='ssh -Nfq -L 2676:simon.mines.edu:22 zstigall@imagine.mines.edu'
alias makeJavaWindowsWork='wmname LG3D'
alias vim=gvim

###Functions
function cd {
if (( $# == 0 )) && [[ `pwd` != $HOME ]]
    then builtin pushd -q ~/
    else builtin cd $*
    fi
}

rationalise-dot() {
    if [[ $LBUFFER = *..  ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

###Path Additions
#Perl Crap
export PERL_LOCAL_LIB_ROOT="/home/zirro/perl5";
export PERL_MB_OPT="--install_base /home/zirro/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/zirro/perl5";
export PERL5LIB="/home/zirro/perl5/lib/perl5/x86_64-linux-thread-multi:/home/zirro/perl5/lib/perl5";
#export PATH="/home/zirro/perl5/bin:$PATH";

#export PATH="${PATH}:/home/zirro/ap/android-sdk-linux/tools:/home/zirro/ap/android-sdk-linux/platform-tools";  #Android SDK
#Cabal Packages
#export PATH="/home/zirro/.cabal/bin:${PATH}";

#More Path
export PATH="${PATH}:/home/zirro/.gem/ruby/1.9.1/bin:/home/zirro/.gem/ruby/1.8/bin"
export PATH="/home/zirro/app/bin:${PATH}";

#Various Exports
export EDITOR="vim";
export BROWSER="firefox";
export WINEARCH="win32";

#CSCI410 Tecs HW Sim
function tecshw {
    /home/zirro/csm/f12/csci410/tecs/HardwareSimulator.sh $PWD/$1
}
