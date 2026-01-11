# Sandbox indicator for shell prompt
# Detects SANDBOX_ACTIVE env var set by sandbox-enter

if [[ -n "$SANDBOX_ACTIVE" ]]; then
    # Function to output sandbox indicator (for lean prompt)
    prompt_sandbox_indicator() {
        echo "%F{yellow}[sandbox]%f "
    }

    # Check if lean prompt is available and use it, otherwise fall back to PS1
    if type prompt_lean_precmd &>/dev/null; then
        PROMPT_LEAN_LEFT='prompt_sandbox_indicator'
    else
        PS1="[sandbox] $PS1"
    fi
fi
