# History configuration via https://unix.stackexchange.com/a/18443
# No duplicates
export HISTCONTROL=ignoredups:erasedups
# Don't overwrite history file
shopt -s histappend
# Write history after every command
if [[ ! $PROMPT_COMMAND =~ .*history.*$ ]]; then
    # Adapt PROMPT_COMMAND only if it does not yet contain history.
    export PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"
fi

# load shared settings
if [[ -f ~/.sharedshellrc ]]; then
    source ~/.sharedshellrc
fi

# load $HOST specific setting
export HOST=`hostname`
if [[ -f ~/.bashrc-$HOST ]]; then
    source ~/.bashrc-$HOST
fi
