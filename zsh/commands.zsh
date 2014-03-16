# TO BE SOURCED FROM $HOME/.zshrc

###############################################################################
# Lets try out vim mode
###############################################################################
bindkey -v
export KEYTIMEOUT=2     #Removes bad lag when switching to NORMAL

#Some keybindings
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '[3~' delete-char            #Forward Delete Key
bindkey '^r' history-incremental-search-backward   #^r now works - history search
bindkey '^[l' push-line

################################################################################
### Aliases
################################################################################
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -la'
alias p=ls
alias pl=ll
alias allcolors='(x=`tput op` y=`printf %80s`;for i in {0..255};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)'
alias killFlash='~/.killFlash.sh'
alias connectToSimon='ssh -Nfq -L 2676:simon.mines.edu:22 zstigall@imagine.mines.edu'
alias makeJavaWindowsWork='wmname LG3D'
# alias vim=gvim
