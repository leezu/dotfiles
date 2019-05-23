# Set theme
PROMPT_LEAN_TMUX=""  # Disable tmux prompt
fpath=( "$HOME/.zfunctions" $fpath )
autoload -U promptinit; promptinit
prompt lean
