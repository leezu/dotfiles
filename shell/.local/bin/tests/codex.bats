#!/usr/bin/env bats

# Codex Wrapper Tests
# ===================
# Focused coverage for install/update behavior without hitting the network.

setup() {
    ORIGINAL_HOME="$HOME"
    ORIGINAL_PATH="$PATH"

    export HOME="$(mktemp -d)"
    TEST_ROOT="$(mktemp -d)"
    SCRIPT_DIR="$(cd "$(dirname "$BATS_TEST_FILENAME")/.." && pwd)"
    CODEX_SCRIPT="$SCRIPT_DIR/codex"
    MOCK_BIN_DIR="$TEST_ROOT/mock-bin"
    PROJECT_DIR="$TEST_ROOT/project"

    mkdir -p "$MOCK_BIN_DIR" "$TEST_ROOT/state" "$HOME/.local/bin" "$PROJECT_DIR"
    export PATH="$MOCK_BIN_DIR:/usr/bin:/bin"
    export TEST_STATE_DIR="$TEST_ROOT/state"
    export MOCK_LATEST_TAG="rust-v0.114.0"
    export MOCK_BINARY_VERSION="0.114.0"
}

teardown() {
    rm -rf "$TEST_ROOT"
    rm -rf "$HOME"
    HOME="$ORIGINAL_HOME"
    PATH="$ORIGINAL_PATH"
}

install_mock_curl() {
    cat > "$MOCK_BIN_DIR/curl" <<'EOF'
#!/bin/bash
set -euo pipefail

printf '%s\n' "$*" >> "$TEST_STATE_DIR/curl.log"

for arg in "$@"; do
    if [[ "$arg" == http://* || "$arg" == https://* ]]; then
        url="$arg"
    fi
done

if [[ " $* " == *" -w %{url_effective} "* ]]; then
    printf 'https://github.com/openai/codex/releases/tag/%s\n' "$MOCK_LATEST_TAG"
    exit 0
fi

out=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        -o)
            out="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

if [[ -z "${url:-}" || -z "$out" ]]; then
    echo "unexpected curl invocation" >&2
    exit 1
fi

printf 'fake archive for %s\n' "$url" > "$out"
EOF
    chmod +x "$MOCK_BIN_DIR/curl"
}

install_mock_tar() {
    cat > "$MOCK_BIN_DIR/tar" <<'EOF'
#!/bin/bash
set -euo pipefail

printf '%s\n' "$*" >> "$TEST_STATE_DIR/tar.log"

archive=""
dest="."
while [[ $# -gt 0 ]]; do
    case "$1" in
        -xzf)
            archive="$2"
            shift 2
            ;;
        -C)
            dest="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

name="$(basename "$archive" .tar.gz)"
cat > "$dest/$name" <<SCRIPT
#!/bin/bash
echo "codex $MOCK_BINARY_VERSION"
SCRIPT
chmod +x "$dest/$name"
EOF
    chmod +x "$MOCK_BIN_DIR/tar"
}

install_mock_bwrap() {
    cat > "$MOCK_BIN_DIR/bwrap" <<'EOF'
#!/bin/bash
set -euo pipefail

printf '%s\n' "$*" >> "$TEST_STATE_DIR/bwrap.log"
exit 0
EOF
    chmod +x "$MOCK_BIN_DIR/bwrap"
}

write_codex_bin() {
    local version="$1"
    cat > "$HOME/.local/bin/codex-bin" <<EOF
#!/bin/bash
echo "codex $version"
EOF
    chmod +x "$HOME/.local/bin/codex-bin"
}

@test "install-only downloads codex-bin outside a git repo" {
    install_mock_curl
    install_mock_tar

    cd "$TEST_ROOT"
    run "$CODEX_SCRIPT" --install-only

    [ "$status" -eq 0 ]
    [[ "$output" == *"Install/update complete"* ]]
    [ -x "$HOME/.local/bin/codex-bin" ]
    [ "$(cat "$HOME/.cache/codex-wrapper/version")" = "$MOCK_LATEST_TAG" ]
}

@test "existing codex-bin seeds missing version file without downloading" {
    write_codex_bin "0.114.0"
    install_mock_curl
    install_mock_tar
    install_mock_bwrap
    mkdir -p "$PROJECT_DIR/.git"

    cd "$PROJECT_DIR"
    run "$CODEX_SCRIPT" --help

    [ "$status" -eq 0 ]
    [ "$(cat "$HOME/.cache/codex-wrapper/version")" = "$MOCK_LATEST_TAG" ]
    [[ ! -e "$TEST_STATE_DIR/tar.log" ]]
}

@test "stale cached version triggers auto-update on install-only" {
    write_codex_bin "0.100.0"
    install_mock_curl
    install_mock_tar
    mkdir -p "$HOME/.cache/codex-wrapper"
    printf 'rust-v0.100.0\n' > "$HOME/.cache/codex-wrapper/version"

    cd "$TEST_ROOT"
    run "$CODEX_SCRIPT" --install-only

    [ "$status" -eq 0 ]
    [ "$(cat "$HOME/.cache/codex-wrapper/version")" = "$MOCK_LATEST_TAG" ]
    run "$HOME/.local/bin/codex-bin" --version
    [ "$status" -eq 0 ]
    [ "$output" = "codex 0.114.0" ]
}

@test "force-update re-downloads even when version is current" {
    write_codex_bin "0.114.0"
    install_mock_curl
    install_mock_tar
    mkdir -p "$HOME/.cache/codex-wrapper"
    printf '%s\n' "$MOCK_LATEST_TAG" > "$HOME/.cache/codex-wrapper/version"
    printf '%s\n' "$(date +%s)" > "$HOME/.cache/codex-wrapper/last_check"
    export MOCK_BINARY_VERSION="0.114.1"

    cd "$TEST_ROOT"
    run "$CODEX_SCRIPT" --force-update --install-only

    [ "$status" -eq 0 ]
    [ "$(cat "$HOME/.cache/codex-wrapper/version")" = "$MOCK_LATEST_TAG" ]
    run "$HOME/.local/bin/codex-bin" --version
    [ "$status" -eq 0 ]
    [ "$output" = "codex 0.114.1" ]
}
