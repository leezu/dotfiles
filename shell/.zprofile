#
# Executes commands at login pre-zshrc.
#

###############################################################################
# Aliases
###############################################################################
# Autossh port forwards
alias dycpu1-ports="autossh -M 0 -f -T -N dycpu1-ports"
alias dycpu2-ports="autossh -M 0 -f -T -N dycpu2-ports"
# Always use --no-folding for stow
alias stow="stow --no-folding"

###############################################################################
# Paths
###############################################################################
# Assure MANPATH is set up
if [[ -z "${MANPATH}" ]]; then
    export MANPATH="$(manpath)"
fi

# Local software
mkdir -p $HOME/.local{bin,includes,lib}
export PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export CPATH="$HOME/.local/include${CPATH:+:${CPATH}}" # cpp searches for include files here (like -I )
export LIBRARY_PATH="$HOME/.local/lib${LIBRARY_PATH:+:${LIBRARY_PATH}}" # link time libraries
export LD_LIBRARY_PATH="$HOME/.local/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}" # run time libraries

# NPM
mkdir -p $HOME/.npm/{bin,share/man}
export NPM_PACKAGES="$HOME/.npm"
export PATH="$NPM_PACKAGES/bin${PATH:+:${PATH}}"
export MANPATH="$NPM_PACKAGES/share/man${MANPATH:+:${MANPATH}}"

###############################################################################
# Directory structure
###############################################################################
export PROJECTDIR="$HOME/projects"
export SOFTWAREDIR="$HOME/software"
export DATASETSDIR="$HOME/datasets"

# Research data
export NLTK_DATA="$DATASETSDIR/nltk_data"

###############################################################################
# Configuration
###############################################################################
# Use UTC instead of local time (eg. git AuthorDate and CommitDate)
export TZ=UTC

# Language
if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
    export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# OS-X
if [[ "$OSTYPE" == darwin* ]]; then
    export BROWSER='open'
fi
