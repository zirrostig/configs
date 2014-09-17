# TO BE SOURCED FROM $HOME/.zshrc

################################################################################
# Misc Options
################################################################################
setopt DVORAK               #Dvorak keymap is used for smart spelling correction
setopt C_BASES              #Output hex as 0x... rather than 16#...
setopt C_PRECEDENCES        #Change order of opts to be more c like
# setopt INTERACTIVE_COMMENTS #Allows comments in the shell
unsetopt BEEP               #No Annoying beep
setopt MULTIOS              #Allows multiple io redirects

################################################################################
# Directory Options
################################################################################
setopt AUTO_CD      #cd not necessary anymore

################################################################################
# Completion Options, Expansion, Globbing
################################################################################
setopt AUTO_LIST            #Shows choices on ambiguous
setopt LIST_AMBIGUOUS       #Fills to ambiguous, then lets AUTO_LIST work
setopt LIST_TYPES
setopt BAD_PATTERN
setopt CASE_MATCH
setopt NOMATCH
setopt NUMERIC_GLOB_SORT
# setopt REMATCH_PCRE         #Perl-Style Regex :)
setopt EXTENDED_GLOB
setopt EQUALS               # =exe expands to `which exe`
autoload -U compinit && compinit

################################################################################
# History Settings
################################################################################
HISTFILE=$HOME/.zsh_hist
HISTSIZE=65536
SAVEHIST=65536
setopt HIST_IGNORE_ALL_DUPS #if command is in history, remove older entry, write
                            # newer
setopt HIST_IGNORE_SPACE    #if command begins with space don't write to history
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE        #Don't store history commands
setopt APPEND_HISTORY       #Good for running multiple zsh sessions
                            # simultaneously
setopt BANG_HIST

################################################################################
# Various Exports
################################################################################
export EDITOR="vim"
export BROWSER="firefox"
export WINEARCH="win64"
export READNULLCMD="less"

################################################################################
# Less Pipe
################################################################################
export LESSOPEN="| source-highlight-esc.sh %s"
# export LESSOPEN='|/usr/bin/lesspipe.sh %s'
export LESS=" -R "

###############################################################################
# Load Colors in linux TTY
###############################################################################
if [[ "$TERM" == "linux" ]]; then
    /bin/bash /home/zirro/cfg/colors.sh
fi

