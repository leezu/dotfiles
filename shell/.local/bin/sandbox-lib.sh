#!/bin/bash
# Sandbox Library
# ===============
#
# Common functionality for bubblewrap sandboxing, shared by:
# - sandbox-enter: Interactive sandboxed shell
# - claude: Claude Code wrapper
# - (future wrappers for other tools)
#
# Usage:
#   source sandbox-lib.sh
#
# This library provides:
# - Dependency checking
# - Project root detection
# - System bind mount arrays
# - Helper functions for building bwrap commands
#
# The library populates arrays that the caller can use/extend:
# - SANDBOX_SYSTEM_BINDS: Core system directories (ro)
# - SANDBOX_OPTIONAL_BINDS: Common optional mounts (dotfiles, git config, etc.)
#
# Callers should:
# 1. Source this library
# 2. Call sandbox_init to populate base arrays
# 3. Add their specific mounts to SANDBOX_OPTIONAL_BINDS
# 4. Call sandbox_build_cmd to build a bwrap command array

# ============================================================================
# Dependency Checking
# ============================================================================

sandbox_check_deps() {
    local deps=("$@")
    for dep in "${deps[@]}"; do
        command -v "$dep" &>/dev/null || {
            echo "Error: $dep not found" >&2
            return 1
        }
    done
}

# ============================================================================
# Project Root Detection
# ============================================================================

# Find project root by looking for marker files
# Arguments: marker files to search for (default: .git)
# Returns: project root path on stdout, or returns 1 if not found
sandbox_find_project_root() {
    local markers=("$@")
    [[ ${#markers[@]} -eq 0 ]] && markers=(".git")

    local d="$PWD"
    local result=""
    while [[ "$d" != "/" ]]; do
        for marker in "${markers[@]}"; do
            if [[ -e "$d/$marker" ]]; then
                result="$d"
                break  # Found a match at this level, continue upward
            fi
        done
        d="$(dirname "$d")"
    done
    [[ -n "$result" ]] && echo "$result" && return 0
    return 1
}

# ============================================================================
# Bind Mount Helpers
# ============================================================================

# Add read-only bind mount if path exists
# Arguments: path [dest] (dest defaults to path)
sandbox_add_ro_bind() {
    local src="$1"
    local dest="${2:-$1}"
    [[ -e "$src" ]] || return 0
    SANDBOX_OPTIONAL_BINDS+=(--ro-bind "$src" "$dest")
}

# Add writable bind mount if path exists
# Arguments: path [dest] (dest defaults to path)
sandbox_add_bind() {
    local src="$1"
    local dest="${2:-$1}"
    [[ -e "$src" ]] || return 0
    SANDBOX_OPTIONAL_BINDS+=(--bind "$src" "$dest")
}

# Add device bind mount if path exists
# Arguments: path [dest] (dest defaults to path)
sandbox_add_dev_bind() {
    local src="$1"
    local dest="${2:-$1}"
    [[ -e "$src" ]] || return 0
    SANDBOX_DEVICE_BINDS+=(--dev-bind "$src" "$dest")
}

# Add device bind mount (try variant - no error if missing)
# Arguments: path [dest] (dest defaults to path)
sandbox_add_dev_bind_try() {
    local src="$1"
    local dest="${2:-$1}"
    SANDBOX_DEVICE_BINDS+=(--dev-bind-try "$src" "$dest")
}

# ============================================================================
# System Bind Mounts
# ============================================================================

# Build core system bind mounts (read-only)
sandbox_build_system_binds() {
    SANDBOX_SYSTEM_BINDS=(
        --ro-bind /usr /usr
        --ro-bind /lib /lib
        --ro-bind /bin /bin
        --ro-bind /sbin /sbin
        --ro-bind /etc /etc
    )

    # Architecture-specific library directories
    [[ -d /lib64 ]] && SANDBOX_SYSTEM_BINDS+=(--ro-bind /lib64 /lib64) || true
    [[ -d /lib32 ]] && SANDBOX_SYSTEM_BINDS+=(--ro-bind /lib32 /lib32) || true
}

# ============================================================================
# Common Optional Binds
# ============================================================================

# Add common optional bind mounts that most sandboxed tools need
sandbox_build_common_binds() {
    SANDBOX_OPTIONAL_BINDS=()

    # Dotfiles directory (read-only)
    sandbox_add_ro_bind "$HOME/.dotfiles"

    # Git config (read-only)
    sandbox_add_ro_bind "$HOME/.gitconfig"
    sandbox_add_ro_bind "$HOME/.config/git"

    # DNS resolution
    sandbox_add_ro_bind "/run/systemd/resolve"
}

# Add shell configuration files (read-only)
sandbox_add_shell_configs() {
    local configs=(
        "$HOME/.zshrc" "$HOME/.zshenv" "$HOME/.zlogin"
        "$HOME/.shellrc" "$HOME/.shell" "$HOME/.zfunctions"
        "$HOME/.bashrc" "$HOME/.bash_profile" "$HOME/.profile"
    )
    for path in "${configs[@]}"; do
        sandbox_add_ro_bind "$path"
    done
}

# Add Rust toolchain (read-only)
# Unlike package caches, rustup contains the actual compiler and tools
sandbox_add_rust_toolchain() {
    sandbox_add_ro_bind "$HOME/.rustup"
    sandbox_add_ro_bind "$HOME/.cargo"
}

# ============================================================================
# GPU Device Passthrough
# ============================================================================

# Add GPU device mounts for NVIDIA/AMD
sandbox_add_gpu_devices() {
    SANDBOX_DEVICE_BINDS=()

    # DRI (Direct Rendering Infrastructure)
    sandbox_add_dev_bind_try /dev/dri /dev/dri
    sandbox_add_dev_bind_try /dev/kfd /dev/kfd

    # NVIDIA devices (up to 8 GPUs)
    for i in {0..7}; do
        sandbox_add_dev_bind_try "/dev/nvidia$i" "/dev/nvidia$i"
    done
    sandbox_add_dev_bind_try /dev/nvidiactl /dev/nvidiactl
    sandbox_add_dev_bind_try /dev/nvidia-uvm /dev/nvidia-uvm
    sandbox_add_dev_bind_try /dev/nvidia-uvm-tools /dev/nvidia-uvm-tools
    sandbox_add_dev_bind_try /dev/nvidia-modeset /dev/nvidia-modeset
    sandbox_add_dev_bind_try /dev/nvidia-caps /dev/nvidia-caps

    # NVIDIA sysfs (read-only)
    SANDBOX_OPTIONAL_BINDS+=(--ro-bind-try /sys/module/nvidia /sys/module/nvidia)
    SANDBOX_OPTIONAL_BINDS+=(--ro-bind-try /sys/module/nvidia_uvm /sys/module/nvidia_uvm)
    SANDBOX_OPTIONAL_BINDS+=(--ro-bind-try /sys/bus/pci /sys/bus/pci)
    SANDBOX_OPTIONAL_BINDS+=(--ro-bind-try /sys/devices/system /sys/devices/system)

    # NVIDIA procfs
    SANDBOX_DEVICE_BINDS+=(--dev-bind-try /proc/driver/nvidia /proc/driver/nvidia)
}

# ============================================================================
# Persistent Home
# ============================================================================

# Get the persistent home directory path for a project
# Arguments: project_root
# Returns: path to persistent home directory on stdout
sandbox_get_persistent_home() {
    local project_root="$1"
    # URL-style escaping: % -> %25, - -> %2d, then / -> -
    local escaped="${project_root//%/%25}"
    escaped="${escaped//-/%2d}"
    escaped="${escaped//\//-}"
    echo "$HOME/.local/state/sandbox/$escaped"
}

# ============================================================================
# Git Worktree Support
# ============================================================================

# Get the main git directory for worktree support
# Returns the repo root containing .git directory, or empty if not a worktree
sandbox_get_worktree_main() {
    local d="$PWD"
    while [[ "$d" != "/" ]]; do
        if [[ -f "$d/.git" ]]; then
            # It's a worktree - .git is a file pointing to the main repo
            local gitdir
            gitdir=$(sed -n 's/^gitdir: //p' "$d/.git")
            [[ -z "$gitdir" ]] && return

            # Convert to absolute path if relative
            if [[ "$gitdir" != /* ]]; then
                gitdir="$d/$gitdir"
            fi

            # Resolve to canonical path and get main .git dir
            # /path/to/repo/.git/worktrees/branch -> /path/to/repo/.git
            gitdir=$(cd "$(dirname "$gitdir")" && pwd)/$(basename "$gitdir")
            local main_gitdir="${gitdir%/worktrees/*}"

            # Return the directory containing .git (the repo root)
            echo "${main_gitdir%/.git}"
            return
        elif [[ -d "$d/.git" ]]; then
            # Regular repo, not a worktree
            return
        fi
        d="$(dirname "$d")"
    done
}

# ============================================================================
# Argument Parsing
# ============================================================================

# Parse a single sandbox option. Sets SANDBOX_SHIFT to args consumed.
# Returns: 0=handled, 1=not sandbox option, 2=error
sandbox_parse_option() {
    SANDBOX_SHIFT=0
    case "$1" in
        --bind)
            [[ $# -lt 3 ]] && { echo "Error: --bind requires SRC DEST" >&2; return 2; }
            SANDBOX_CUSTOM_BINDS+=(--bind "$2" "$3")
            SANDBOX_SHIFT=3 ;;
        --ro-bind)
            [[ $# -lt 3 ]] && { echo "Error: --ro-bind requires SRC DEST" >&2; return 2; }
            SANDBOX_CUSTOM_BINDS+=(--ro-bind "$2" "$3")
            SANDBOX_SHIFT=3 ;;
        --persistent-home)
            SANDBOX_PERSISTENT_HOME=true
            SANDBOX_SHIFT=1 ;;
        --gpu)
            SANDBOX_GPU=true
            SANDBOX_SHIFT=1 ;;
        --worktree-rw)
            SANDBOX_WORKTREE_RW=true
            SANDBOX_SHIFT=1 ;;
        *)
            return 1 ;;
    esac
    return 0
}

# ============================================================================
# Initialization
# ============================================================================

# Initialize sandbox arrays with common configuration
# Call this before adding custom mounts
# Note: SANDBOX_CUSTOM_BINDS is preserved if already set (for early arg parsing)
sandbox_init() {
    sandbox_build_system_binds
    sandbox_build_common_binds
    SANDBOX_DEVICE_BINDS=()
    # Preserve SANDBOX_CUSTOM_BINDS if already populated by argument parsing
    [[ -v SANDBOX_CUSTOM_BINDS ]] || SANDBOX_CUSTOM_BINDS=()
    SANDBOX_ENV_VARS=(--setenv TZ UTC)
}

# Build the bwrap command array without executing
# Populates SANDBOX_BWRAP_CMD array
# Arguments:
#   $1 - project/working directory to bind writable
#   $2 - home mode: "ephemeral" or "bind:/path/to/home"
#   $@ - command and arguments to run inside sandbox
sandbox_build_cmd() {
    local project_dir="$1"
    local home_mode="$2"
    shift 2
    local cmd=("$@")

    SANDBOX_BWRAP_CMD=(
        bwrap
        "${SANDBOX_SYSTEM_BINDS[@]}"
        --tmpfs /tmp
        --tmpfs /run
        --proc /proc
        --dev /dev
        "${SANDBOX_DEVICE_BINDS[@]}"
    )

    # Add home directory based on mode
    if [[ "$home_mode" == "ephemeral" ]]; then
        SANDBOX_BWRAP_CMD+=(--dir "$HOME")
    elif [[ "$home_mode" == bind:* ]]; then
        local home_path="${home_mode#bind:}"
        SANDBOX_BWRAP_CMD+=(--bind "$home_path" "$HOME")
    else
        echo "Error: invalid home_mode '$home_mode' (expected 'ephemeral' or 'bind:/path')" >&2
        return 1
    fi

    SANDBOX_BWRAP_CMD+=(
        "${SANDBOX_OPTIONAL_BINDS[@]}"
        --bind "$project_dir" "$project_dir"
        "${SANDBOX_CUSTOM_BINDS[@]}"
        "${SANDBOX_ENV_VARS[@]}"
        --chdir "$(pwd)"
        --unshare-user
        --unshare-ipc
        --unshare-uts
        --unshare-cgroup
        --die-with-parent
        --
        "${cmd[@]}"
    )
}
