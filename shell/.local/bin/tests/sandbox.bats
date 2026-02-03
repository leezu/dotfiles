#!/usr/bin/env bats

# Sandbox Library Tests
# =====================
# Minimal test coverage for critical sandbox functionality.
# Run with: bats shell/.local/bin/tests/sandbox.bats

# Setup: create temp project dir
setup() {
    SCRIPT_DIR="$(cd "$(dirname "$BATS_TEST_FILENAME")/.." && pwd)"
    source "$SCRIPT_DIR/sandbox-lib.sh"

    TEST_PROJECT="$(mktemp -d)"
    mkdir -p "$TEST_PROJECT/.git"
}

teardown() {
    rm -rf "$TEST_PROJECT"
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

@test "sandbox_parse_option returns 1 for unknown option" {
    run sandbox_parse_option --unknown
    [ "$status" -eq 1 ]
}
