# History configuration via https://www.digitalocean.com/community/tutorials/how-to-use-bash-history-commands-and-expansions-on-a-linux-vps
# Don't overwrite history file
shopt -s histappend
# Write history after every command
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# load $HOST specific setting
export HOST=`hostname`
if [[ -f ~/.bashrc-$HOST ]]; then
    source ~/.bashrc-$HOST
fi
