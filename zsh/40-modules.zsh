# TO BE SOURCED FROM $HOME/.zshrc

################################################################################
# Zsh Specials
################################################################################
autoload -U zmv
autoload -U tetris && zle -N tetris   #Because we can

#Bash like command line edit
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

#Setup cdr
autoload -U add-zsh-hook
autoload -U cdr
autoload -U chpwd_recent_dirs
add-zsh-hook chpwd chpwd_recent_dirs

################################################################################
### Zsh Modules
################################################################################
# zmodload zsh/pcre       #pcre_compile and pcre_match
zmodload zsh/datetime
zmodload zsh/mathfunc
