## Aliases
# Dictionaries
alias ddevil='dict -d devil'
alias wn='dict -d wn'
alias ee='dict -d moby-thesaurus'
alias ed='dict -d fd-eng-deu'
alias de='dict -d fd-deu-eng'
function ze
  grep $argv ~/.cedict.txt
end
function zes --description 'Search for single-character simplified Chinese phrase'
  grep '^. '$argv ~/.cedict.txt
end
function zed --description 'Search for two-character simplified Chinese phrase'
  grep '^.. '$argv ~/.cedict.txt
end

# Info pages and manuals
alias info='info --vi-keys'
# Toggle capslock
alias capslock_toggle="python -c 'from ctypes import *; X11 = cdll.LoadLibrary("libX11.so.6"); display = X11.XOpenDisplay(None); X11.XkbLockModifiers(display, c_uint(0x0100), c_uint(2), c_uint(0)); X11.XCloseDisplay(display)'"

## Variables
## fish automatically creates arrays from the variables PATH, CDPATH and MANPATH when it is started
# Local software
set PATH $HOME/.local/bin $PATH
set -x CPATH "$HOME/.local/include:$CPATH" # cpp searches for include files here (like -I )
set -x LIBRARY_PATH "$HOME/.local/lib:$LIBRARY_PATH" # link time libraries
set -x LD_LIBRARY_PATH "$HOME/.local/lib:$LD_LIBRARY_PATH" # run time libraries

# NPM
set -x NPM_PACKAGES "$HOME/develop/npm-packages"
set PATH $NPM_PACKAGES/bin $PATH
set -e MANPATH
set -x MANPATH $NPM_PACKAGES/share/man (manpath)
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

# Ruby gems
set PATH /home/leonard/.gem/ruby/2.3.0/bin $PATH

# Preferred editor for local and remote sessions
# TODO implement this once using fish shell on dycpu: set default to vim then
set -x EDITOR "emacsclient -c --tty --alternate-editor=''"
alias emacs="emacsclient -c --tty --alternate-editor=''"

# Source host specific configuration
source ~/.config/fish/config-(hostname).fish
