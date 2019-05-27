# Local software
mkdir -p $HOME/.local/{bin,includes,lib}
export PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export CPATH="$HOME/.local/include${CPATH:+:${CPATH}}" # cpp searches for include files here (like -I )
export LIBRARY_PATH="$HOME/.local/lib${LIBRARY_PATH:+:${LIBRARY_PATH}}" # link time libraries
export LD_LIBRARY_PATH="$HOME/.local/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}" # run time libraries