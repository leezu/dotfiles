#!/usr/bin/env bats

# Sandbox Library Tests
# =====================
# Minimal test coverage for critical sandbox functionality.
# Run with: bats shell/.local/bin/tests/sandbox.bats

# Setup: create temp project dir
setup() {
    ORIGINAL_HOME="$HOME"
    HOME="$(mktemp -d)"

    SCRIPT_DIR="$(cd "$(dirname "$BATS_TEST_FILENAME")/.." && pwd)"
    source "$SCRIPT_DIR/sandbox-lib.sh"

    TEST_PROJECT="$(mktemp -d)"
    mkdir -p "$TEST_PROJECT/.git"
}

teardown() {
    rm -rf "$TEST_PROJECT"
    rm -rf "$HOME"
    HOME="$ORIGINAL_HOME"
}

# === Unit Tests: sandbox_get_persistent_home ===

@test "persistent home: simple path" {
    run sandbox_get_persistent_home "/home/user/project"
    [ "$status" -eq 0 ]
    [ "$output" = "$HOME/.local/state/sandbox/-home-user-project" ]
}

@test "persistent home: path with dashes" {
    run sandbox_get_persistent_home "/home/user/my-project"
    [ "$status" -eq 0 ]
    [ "$output" = "$HOME/.local/state/sandbox/-home-user-my%2dproject" ]
}

@test "persistent home: path with percent" {
    run sandbox_get_persistent_home "/home/user/test%dir"
    [ "$status" -eq 0 ]
    [ "$output" = "$HOME/.local/state/sandbox/-home-user-test%25dir" ]
}

# === Unit Tests: sandbox_find_project_root ===

@test "find project root: in git repo" {
    mkdir -p "$TEST_PROJECT/subdir"
    cd "$TEST_PROJECT/subdir"
    run sandbox_find_project_root .git
    [ "$status" -eq 0 ]
    [ "$output" = "$TEST_PROJECT" ]
}

@test "find project root: outside project" {
    cd /tmp
    run sandbox_find_project_root .git .sandbox-project
    [ "$status" -eq 1 ]
}

@test "find project root: sandbox-project marker" {
    local marker_root
    marker_root="$(mktemp -d)"
    mkdir -p "$marker_root/subdir"
    touch "$marker_root/.sandbox-project"
    cd "$marker_root/subdir"
    run sandbox_find_project_root .git .sandbox-project
    [ "$status" -eq 0 ]
    [ "$output" = "$marker_root" ]
    rm -rf "$marker_root"
}

@test "find project root: nested repos finds highest" {
    mkdir -p "$TEST_PROJECT/nested/.git"
    cd "$TEST_PROJECT/nested"
    run sandbox_find_project_root .git
    [ "$status" -eq 0 ]
    [ "$output" = "$TEST_PROJECT" ]  # Should find outer, not inner
}

# === Unit Tests: sandbox_get_worktree_main ===

@test "worktree main: valid gitdir returns main .git path" {
    local main_repo="$TEST_PROJECT/main-repo"
    local worktree="$TEST_PROJECT/wt-valid"
    mkdir -p "$main_repo/.git/worktrees/wt-valid" "$worktree"
    echo "gitdir: $main_repo/.git/worktrees/wt-valid" > "$worktree/.git"

    cd "$worktree"
    run sandbox_get_worktree_main
    [ "$status" -eq 0 ]
    [ "$output" = "$main_repo/.git" ]
}

@test "worktree main: invalid gitdir outside worktrees is rejected" {
    local worktree="$TEST_PROJECT/wt-invalid-etc"
    mkdir -p "$worktree"
    echo "gitdir: /etc" > "$worktree/.git"

    cd "$worktree"
    run sandbox_get_worktree_main
    [ "$status" -eq 1 ]
}

@test "worktree main: invalid relative gitdir is rejected" {
    local worktree="$TEST_PROJECT/wt-invalid-relative"
    mkdir -p "$worktree"
    echo "gitdir: ../not-a-worktree" > "$worktree/.git"

    cd "$worktree"
    run sandbox_get_worktree_main
    [ "$status" -eq 1 ]
}

@test "worktree main: regular repo returns empty" {
    cd "$TEST_PROJECT"
    run sandbox_get_worktree_main
    [ "$status" -eq 0 ]
    [ -z "$output" ]
}

# === Unit Tests: sandbox_build_cmd ===

@test "sandbox_build_cmd includes project dir" {
    cd "$TEST_PROJECT"
    sandbox_init
    sandbox_build_cmd "$TEST_PROJECT" "ephemeral" bash
    [[ " ${SANDBOX_BWRAP_CMD[*]} " == *" $TEST_PROJECT "* ]]
}

@test "sandbox_build_cmd with persistent home includes bind" {
    cd "$TEST_PROJECT"
    sandbox_init
    local persistent_home
    persistent_home=$(sandbox_get_persistent_home "$TEST_PROJECT")
    mkdir -p "$persistent_home"
    sandbox_build_cmd "$TEST_PROJECT" "bind:$persistent_home" bash
    [[ " ${SANDBOX_BWRAP_CMD[*]} " == *"--bind"* ]]
    [[ " ${SANDBOX_BWRAP_CMD[*]} " == *"$persistent_home"* ]]
    rm -rf "$persistent_home"
}

# === Integration Tests: sandbox-enter ===

@test "read-only parent preserves writable project and read-only siblings and children" {
    command -v bwrap >/dev/null || skip "bubblewrap unavailable"
    bwrap --ro-bind / / --unshare-user -- /bin/true 2>/dev/null || skip "bubblewrap unavailable in this environment"

    local parent="$TEST_PROJECT/Tax"
    local project="$parent/US FBAR"
    mkdir -p "$project/references" "$project/subdir" "$parent/returns"
    printf 'return data\n' > "$parent/returns/input"
    cd "$project/subdir"
    sandbox_init
    # A trailing slash must not prevent recognizing a parent destination.
    SANDBOX_CUSTOM_BINDS=(
        --ro-bind "$parent" "$parent/"
        --ro-bind "$project/references" "$project/references"
    )
    local home_mode
    for home_mode in ephemeral "bind:$HOME"; do
        sandbox_build_cmd "$project" "$home_mode" /bin/bash -c '
            set -e
            printf "saved\n" > output
            cat "$1/returns/input"
            if touch "$1/returns/forbidden" 2>/dev/null; then exit 1; fi
            if touch "$2/references/forbidden" 2>/dev/null; then exit 1; fi
        ' bash "$parent" "$project"
        run "${SANDBOX_BWRAP_CMD[@]}"
        [ "$status" -eq 0 ]
        [ "$output" = "return data" ]
        [ "$(cat "$project/subdir/output")" = saved ]
        rm "$project/subdir/output"
    done
}

@test "explicit read-only project mount still takes precedence" {
    cd "$TEST_PROJECT"
    sandbox_init
    SANDBOX_CUSTOM_BINDS=(--ro-bind "$TEST_PROJECT" "$TEST_PROJECT")
    sandbox_build_cmd "$TEST_PROJECT" ephemeral bash
    [[ " ${SANDBOX_BWRAP_CMD[*]} " == *"--bind $TEST_PROJECT $TEST_PROJECT --ro-bind $TEST_PROJECT $TEST_PROJECT"* ]]
}

@test "sandbox-enter outside project fails" {
    cd /tmp
    run "$SCRIPT_DIR/sandbox-enter"
    [ "$status" -eq 1 ]
    [[ "$output" == *"not in a project"* ]]
}

# === Unit Tests: sandbox_parse_option ===

@test "sandbox_parse_option handles --bind with validation" {
    SANDBOX_CUSTOM_BINDS=()
    sandbox_parse_option --bind /src /dest
    [ "$?" -eq 0 ]
    [ "$SANDBOX_SHIFT" -eq 3 ]
    [[ " ${SANDBOX_CUSTOM_BINDS[*]} " == *"--bind /src /dest"* ]]
}

@test "sandbox_parse_option --bind requires SRC DEST" {
    run sandbox_parse_option --bind /src
    [ "$status" -eq 2 ]
    [[ "$output" == *"requires SRC DEST"* ]]
}

@test "sandbox_parse_option handles --gpu" {
    SANDBOX_GPU=false
    sandbox_parse_option --gpu
    [ "$?" -eq 0 ]
    [ "$SANDBOX_SHIFT" -eq 1 ]
    [ "$SANDBOX_GPU" = "true" ]
}

@test "sandbox_parse_option handles --neuron" {
    SANDBOX_NEURON=false
    sandbox_parse_option --neuron
    [ "$?" -eq 0 ]
    [ "$SANDBOX_SHIFT" -eq 1 ]
    [ "$SANDBOX_NEURON" = "true" ]
}

@test "sandbox_parse_option handles --dev" {
    SANDBOX_DEV=false
    sandbox_parse_option --dev
    [ "$?" -eq 0 ]
    [ "$SANDBOX_SHIFT" -eq 1 ]
    [ "$SANDBOX_DEV" = "true" ]
}

@test "sandbox_parse_option returns 1 for unknown option" {
    run sandbox_parse_option --unknown
    [ "$status" -eq 1 ]
}
