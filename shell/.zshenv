# Sourced on all invocations of zsh

# Load all files from .shell/env.d directory
if [ -d $HOME/.shellrc/env.d ]; then
    for file in $HOME/.shellrc/env.d/*.sh; do
        source $file
    done
fi
