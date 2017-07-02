# Enabling on a new server

- Clone repository to `~/.dotfiles`
- Run `mkdir ~/.config/nvim`
- Run `mkdir ~/.local/bin`
- Run `stow DIR` for all directory names in `~/.dotfiles`

## zsh
- Install `oh-my-zsh`
- Make zsh default with `chsh -s PATH_TO_ZSH`

## vim
- Make sure neovim is used and not vim
- Install `vim-plug` package manager
- Run `:PlugInstall`
- YouCompleteMe should be rebuilt to support autocompletion
- For vimtex the pdf viewer zathura is used

## tmux
- Install `tpm` plugin manager (at least 2.1)
- Press prefix + I (capital I, as in Install) to fetch the plugins

## emacs
- Clone spacemacs repository into `~/.spacemacs.d/`
- Install prerequisites for pdf-tools layer
- Open emacs to install plugins

## R
- `mkdir -p ~/develop/R/3.2`
