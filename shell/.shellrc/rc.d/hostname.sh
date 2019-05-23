export HOST=`hostname`
if [[ -f ~/.sharedshellrc-$HOST ]]; then
    source ~/.sharedshellrc-$HOST
fi
if [[ "$OSTYPE" == linux-gnu ]] && curl -s --max-time 0.01 http://169.254.169.254/latest/dynamic/instance-identity/document | grep -q availabilityZone; then
    source ~/.sharedshellrc-aws
fi