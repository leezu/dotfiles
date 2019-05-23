source ~/.shell/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.shell/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '\e[A' history-substring-search-up
bindkey '\e[B' history-substring-search-down
bindkey -s '\eOA' '\e[A'  # Some terminals send '\eOA' instead of '\e[A'
bindkey -s '\eOB' '\e[B'  # Some terminals send '\eOB' instead of '\e[B'

# History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zhistory"
HISTSIZE=50000
SAVEHIST=10000

# History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history # share command history data
