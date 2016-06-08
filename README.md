# Enabling on a new server

- Clone repository to `~/.dotfiles`
- Run `stow --ignore=README.md .` in `~/.dotfiles`

## zsh
- Install `oh-my-zsh`
- Make zsh default with `chsh -s PATH_TO_ZSH`

## vim
- Make sure neovim is used and not vim
- Install `vim-plug` package manager
- Run `:PlugInstall`
- YouCompleteMe should be rebuilt to support autocompletion

## tmux
- Install `tpm` plugin manager (at least 2.1)
- Press prefix + I (capital I, as in Install) to fetch the plugins
