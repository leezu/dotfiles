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

# Set KEYTIMEOUT to 0.1 seconds to speed up switching between normal
# and insert mode
export KEYTIMEOUT=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
