#
# Executes commands in interactive shells.
#

# Warn for unsent mails
if [ -d "$HOME/.local/share/mail.queue" ]; then
    mailcount=$(ls -1 ~/.local/share/mail.queue | wc -l)
    if [ $mailcount -gt 0 ] ; then
        echo -e "\e[31m$mailcount unsent mails"
    fi
fi

# Set theme
PROMPT_LEAN_TMUX=""  # Disable tmux prompt
fpath=( "$HOME/.zfunctions" $fpath )
autoload -U promptinit; promptinit
prompt lean


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

# History & highlighting
source ~/.shell/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.shell/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '\e[A' history-substring-search-up
bindkey '\e[B' history-substring-search-down
bindkey -s '\eOA' '\e[A'  # Some terminals send '\eOA' instead of '\e[A'
bindkey -s '\eOB' '\e[B'  # Some terminals send '\eOB' instead of '\e[B'

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zhistory"
HISTSIZE=50000
SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history # share command history data
