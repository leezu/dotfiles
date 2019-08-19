# Local software
mkdir -p $HOME/local/{bin,includes,lib}
export PATH="$HOME/local/bin${PATH:+:${PATH}}"
export CPATH="$HOME/local/include${CPATH:+:${CPATH}}" # cpp searches for include files here (like -I )
export LIBRARY_PATH="$HOME/local/lib${LIBRARY_PATH:+:${LIBRARY_PATH}}" # link time libraries
export LD_LIBRARY_PATH="$HOME/local/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}" # run time libraries
export PKG_CONFIG_PATH="$HOME/local/lib/pkgconfig:$HOME/local/share/pkgconfig" # make sure pkgconfig can find local packages
export GSETTINGS_SCHEMA_DIR="$HOME/local/share/glib-2.0/schemas"
export XDG_DATA_DIRS="$HOME/local/share${XDG_DATA_DIRS:+:${XDG_DATA_DIRS}}"
