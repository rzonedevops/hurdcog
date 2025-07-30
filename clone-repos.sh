#!/bin/bash
# Script to clone GNU Hurd ecosystem repositories
# Run this script when network access to git.savannah.gnu.org is available

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Cloning GNU Hurd ecosystem repositories..."

# Create directory structure
mkdir -p external/gnu-repos external/hurd-repos

# Clone GNU repositories
echo "Cloning GNU Bash..."
if [ ! -d "external/gnu-repos/bash/.git" ]; then
    cd external/gnu-repos
    git clone https://git.savannah.gnu.org/git/bash.git
    cd ../..
else
    echo "Bash already cloned, updating..."
    cd external/gnu-repos/bash && git pull && cd ../../..
fi

# Clone Hurd repositories
echo "Cloning Hurd ecosystem repositories..."

repos=(
    "hurd.git:hurd-meta"
    "hurd/glibc.git:glibc"
    "hurd/gnumach.git:gnumach"
    "hurd/hurd.git:hurd"
    "hurd/incubator.git:incubator"
    "hurd/libpthread.git:libpthread"
    "hurd/mig.git:mig"
    "hurd/procfs.git:procfs"
    "hurd/unionfs.git:unionfs"
    "hurd/viengoos.git:viengoos"
    "hurd/web.git:web"
)

cd external/hurd-repos

for repo in "${repos[@]}"; do
    IFS=':' read -r repo_path dir_name <<< "$repo"
    echo "Cloning $repo_path to $dir_name..."
    
    if [ ! -d "$dir_name/.git" ]; then
        git clone "https://git.savannah.gnu.org/git/$repo_path" "$dir_name"
    else
        echo "$dir_name already cloned, updating..."
        cd "$dir_name" && git pull && cd ..
    fi
done

cd ../..

echo "Repository cloning completed!"
echo "Note: This script preserves the existing README.md files in each directory."
echo "The actual repository content will be merged with the documentation structure."

# Create a status file to indicate successful cloning
echo "$(date): Repositories successfully cloned" > external/CLONE_STATUS.txt
