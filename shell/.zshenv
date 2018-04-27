#
# Defines environment variables.
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

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
# Local software
export PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export CPATH="$HOME/.local/include${CPATH:+:${CPATH}}" # cpp searches for include files here (like -I )
export LIBRARY_PATH="$HOME/.local/lib${LIBRARY_PATH:+:${LIBRARY_PATH}}" # link time libraries
export LD_LIBRARY_PATH="$HOME/.local/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}" # run time libraries

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
