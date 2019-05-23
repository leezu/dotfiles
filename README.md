# dotfiles

Dotfiles for individual categories (directories in this repository) can be
activated via `stow CATEGORY`.

``` bash
git clone --recurse-submodules https://github.com/leezu/dotfiles/ ~/.dotfiles
cd ~/.dotfiles
```

## Manual steps
- shell
  - `mkdir ~/.shellrc/{bashrc.d,zshrc.d,rc.d,login.d}`
  - `git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`
  - Open tmux, press `prefix + I`
- emacs
  - `git clone https://github.com/syl20bnr/spacemacs.git ~/.emacs.d -b develop`
- mail
  - Make sure `~/.msmtp-password.gpg` is present

