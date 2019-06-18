# Warn for unsent mails
if [ -d "$HOME/.local/share/mail.queue" ]; then
    mailcount=$(ls -1 ~/.local/share/mail.queue | wc -l)
    if [ $mailcount -gt 0 ] ; then
        echo -e "\e[31m$mailcount unsent mails"
    fi
fi

# Warn for syncthing conflicts in ~/org
if [ -d "$HOME/org" ]; then
    conflictcount=$(find $HOME/org -name "*.sync-conflict-*" -not -path "*.stversions*" | wc -l)
    if [ $conflictcount -gt 0 ] ; then
        echo -e "\e[31m$conflictcount *.sync-conflict-* files in ~/org"
    fi
fi

# Warn for syncthing conflicts in ~/wiki
if [ -d "$HOME/wiki" ]; then
    conflictcount=$(find $HOME/wiki -name "*.sync-conflict-*" -not -path "*.stversions*" | wc -l)
    if [ $conflictcount -gt 0 ] ; then
        echo -e "\e[31m$conflictcount *.sync-conflict-* files in ~/wiki"
    fi
fi