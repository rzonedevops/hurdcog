#!/bin/bash
# Script to clone Unicorn-Forest repositories
# This script clones repositories from the Unicorn-Forest GitHub organization
# to address issue #9 and related issues #10-#19

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Function to log with timestamp
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Function to handle errors
error_exit() {
    log "ERROR: $1"
    exit 1
}

log "Starting Unicorn-Forest repository cloning..."

# Create directory structure for Unicorn-Forest repositories
mkdir -p external/unicorn-forest-repos || error_exit "Failed to create unicorn-forest-repos directory"

# Repository mapping based on issues #10-#19
repos=(
    "gnumach:gnumach"           # Issue #10
    "hurd:hurd"                 # Issue #11
    "libpthread:libpthread"     # Issue #12
    "incubator:incubator"       # Issue #13
    "mig:mig"                   # Issue #14
    "procfs:procfs"             # Issue #15
    "unionfs:unionfs"           # Issue #16
    "viengoos:viengoos"         # Issue #17
    "web:web"                   # Issue #18
    "glibc:glibc"               # Issue #19
    "bash:bash"                 # Additional repository found
    "h:h"                       # Additional repository found
)

cd external/unicorn-forest-repos

for repo in "${repos[@]}"; do
    IFS=':' read -r repo_name dir_name <<< "$repo"
    log "Cloning Unicorn-Forest/$repo_name to $dir_name..."
    
    if [ ! -d "$dir_name/.git" ]; then
        if ! git clone --progress "https://github.com/Unicorn-Forest/$repo_name.git" "$dir_name"; then
            error_exit "Failed to clone Unicorn-Forest/$repo_name"
        fi
    else
        log "$dir_name already cloned, updating..."
        cd "$dir_name" && git pull && cd ..
    fi
done

cd ../..

log "Unicorn-Forest repository cloning completed!"
log "All repositories from issues #10-#19 have been successfully cloned."

# Create a status file to indicate successful cloning
echo "$(date): Unicorn-Forest repositories successfully cloned" > external/UNICORN_FOREST_STATUS.txt
echo "Repository mapping:" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #10 (gnumach) → external/unicorn-forest-repos/gnumach" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #11 (hurd) → external/unicorn-forest-repos/hurd" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #12 (libpthread) → external/unicorn-forest-repos/libpthread" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #13 (incubator) → external/unicorn-forest-repos/incubator" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #14 (mig) → external/unicorn-forest-repos/mig" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #15 (procfs) → external/unicorn-forest-repos/procfs" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #16 (unionfs) → external/unicorn-forest-repos/unionfs" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #17 (viengoos) → external/unicorn-forest-repos/viengoos" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #18 (web) → external/unicorn-forest-repos/web" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue #19 (glibc) → external/unicorn-forest-repos/glibc" >> external/UNICORN_FOREST_STATUS.txt
echo "Additional: bash → external/unicorn-forest-repos/bash" >> external/UNICORN_FOREST_STATUS.txt
echo "Additional: h → external/unicorn-forest-repos/h" >> external/UNICORN_FOREST_STATUS.txt

log "Status file created: external/UNICORN_FOREST_STATUS.txt"