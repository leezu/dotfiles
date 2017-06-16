# History configuration via https://www.digitalocean.com/community/tutorials/how-to-use-bash-history-commands-and-expansions-on-a-linux-vps
# Don't overwrite history file
shopt -s histappend
# Write history after every command
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# Aliases
# Dictionaries
alias ddevil='dict -d devil'
alias wn='dict -d wn'
alias ee='dict -d moby-thesaurus'
alias ed='dict -d fd-eng-deu'
alias de='dict -d fd-deu-eng'
function ze() {
    grep $argv ~/.cedict.txt
}
function zes() {
    grep '^. '$argv ~/.cedict.txt
}
function zed() {
    grep '^.. '$argv ~/.cedict.txt
}

# Info pages and manuals
alias info='info --vi-keys'

# Local software
export PATH="$HOME/.local/bin:$PATH"
export CPATH="$HOME/.local/include:$CPATH" # cpp searches for include files here (like -I )
export LIBRARY_PATH="$HOME/.local/lib:$LIBRARY_PATH" # link time libraries
export LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH" # run time libraries

# Create directories
if [[ ! ( -d ~/.local/bin || -d ~/.local/lib || -d ~/./local/include ) ]]; then
    mkdir -p ~/.local/{bin,lib,include}
fi

# load $HOST specific setting
export HOST=`hostname`
if [[ -f ~/.bashrc-$HOST ]]; then
    source ~/.bashrc-$HOST
fi
