# load $HOST specific setting
if [[ -f ~/.profile-$HOST ]]; then
    source ~/.profile-$HOST
fi

export PATH="$HOME/.cargo/bin:$PATH"
