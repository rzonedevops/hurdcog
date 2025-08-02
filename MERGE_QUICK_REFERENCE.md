# Quick Reference: Manual Merge Process

This document provides quick access to the manual merge tools for resolving conflicts between the `clone-me` branch and `master`.

## Quick Start

1. **Start the merge:**
   ```bash
   git checkout clone-me
   git merge master --allow-unrelated-histories
   ```

2. **Resolve conflicts automatically:**
   ```bash
   ./resolve-clone-me-conflicts.sh
   ```

3. **Commit the merge:**
   ```bash
   git commit -m "Merge master into clone-me - resolve conflicts"
   git push -u origin clone-me
   ```

## Files

- [`MANUAL_MERGE_GUIDE.md`](MANUAL_MERGE_GUIDE.md) - Complete step-by-step documentation
- [`resolve-clone-me-conflicts.sh`](resolve-clone-me-conflicts.sh) - Automated conflict resolution script

## Expected Conflicts

The merge process will encounter conflicts in these files:
- `.github/scripts/*.py` - Python automation scripts (keep clone-me version)
- `.github/workflows/*.yml` - GitHub Actions workflows (keep clone-me version)  
- `clone-repos.sh` - Repository cloning script (keep enhanced clone-me version)
- `Makefile` - Build configuration (intelligent merge)
- `external/README.md` - Documentation (combine both versions)

## Manual Resolution Required

If the automated script cannot resolve all conflicts, manually resolve remaining files and then:

```bash
git add <filename>
git commit
```

See the full documentation in [MANUAL_MERGE_GUIDE.md](MANUAL_MERGE_GUIDE.md) for detailed instructions.