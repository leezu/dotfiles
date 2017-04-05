# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#plugins=(vi-mode git)
plugins=(git)

# Fix glitches of vi-mode
bindkey '\e[A' up-line-or-search
bindkey '\e[B' down-line-or-search




###
### User configuration
###

## Aliases
# Dictionaries
alias ddevil='dict -d devil'
alias wn='dict -d wn'
alias ee='dict -d moby-thesaurus'
alias ed='dict -d fd-eng-deu'
alias de='dict -d fd-deu-eng'
# Info pages and manuals
alias info='info --vi-keys'

# Simplify shell for emacs tramp
if [[ $TERM == "dumb" ]]; then	# in emacs
    PS1='%(?..[%?])%!:%~%# '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
else
    source $ZSH/oh-my-zsh.sh
fi

# Local software
PATH="$HOME/.local/bin:$PATH"
CPATH="$HOME/.local/include:$CPATH" # cpp searches for include files here (like -I )
LIBRARY_PATH="$HOME/.local/lib:$LIBRARY_PATH" # link time libraries
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH" # run time libraries

# NPM
NPM_PACKAGES="$HOME/develop/npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"
unset MANPATH
MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

# Ruby gems
PATH="/home/leonard/.gem/ruby/2.3.0/bin:$PATH"

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR="emacsclient -c --tty --alternate-editor=''"
 fi
 alias emacs="emacsclient -c --tty --alternate-editor=''"

# Set KEYTIMEOUT to 0.1 seconds to speed up switching between normal
# and insert mode
export KEYTIMEOUT=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# load $HOST specific setting
if [[ -f ~/.zshrc-$HOST ]]; then
    source ~/.zshrc-$HOST
fi
