# This file is sourced by zsh for login shells.

if [[ -f ~/.profile ]] ; then
    emulate sh -c '. ~/.profile'
fi

if [[ -f ~/.zshrc ]] ; then
    . ~/.zshrc
fi
