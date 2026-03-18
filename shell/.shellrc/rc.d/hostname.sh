export HOST=`hostname`
if [[ -f ~/.sharedshellrc-$HOST ]]; then
    source ~/.sharedshellrc-$HOST
fi
if [[ "$OSTYPE" == linux-gnu ]] && curl -sf --max-time 0.01 -X PUT "http://169.254.169.254/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 1" -o /dev/null 2>/dev/null; then
    source ~/.sharedshellrc-aws
fi
