# dotfiles

Dotfiles for individual categories (directories in this repository) can be
activated via `stow CATEGORY`.

``` bash
git clone --recurse-submodules https://github.com/leezu/dotfiles/ ~/.dotfiles
cd ~/.dotfiles
```

Run `stow` with `--no-folding` to allow multiple stow directories.

## Manual steps
- shell
  - `mkdir -p ~/.shellrc/{bashrc.d,zshrc.d,rc.d,env.d}`
  - `git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`
  - Open tmux, press `prefix + I`
- desktop
  - To automatically switch to power-saver profile on battery, use `systemctl --user enable --now power_monitor.timer`
- emacs
  - `git clone https://github.com/syl20bnr/spacemacs.git ~/.emacs.d -b develop`
- mail
  - Make sure `~/.msmtp-password.gpg` is present

