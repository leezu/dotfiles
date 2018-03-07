# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# User configuration
# load shared settings
if [[ -f ~/.sharedshellrc ]]; then
    source ~/.sharedshellrc
fi

# load OS specific settings
if [[ "$OSTYPE" == darwin* ]]; then
    source ~/.zshrc-osx
fi

# load $HOST specific setting
if [[ -f ~/.zshrc-$HOST ]]; then
    source ~/.zshrc-$HOST
fi


# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    alias emacs="emacsclient -c --tty --alternate-editor=''"
    export EDITOR="emacs"
fi

# Set KEYTIMEOUT to 0.1 seconds to speed up switching between normal
# and insert mode
export KEYTIMEOUT=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
