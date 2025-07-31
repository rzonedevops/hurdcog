# Unicorn-Forest Repositories Mapping

This document maps the issues #10-#19 to their corresponding Unicorn-Forest repositories, addressing issue #9.

## Source
- **Organization**: [Unicorn-Forest](https://github.com/Unicorn-Forest)
- **Related Issue**: #9 - "Unicorn-Forest repositories"

## Repository Mapping

| Issue | Repository Name | Unicorn-Forest URL | Local Path |
|-------|----------------|-------------------|------------|
| #10   | gnumach        | https://github.com/Unicorn-Forest/gnumach | `external/unicorn-forest-repos/gnumach` |
| #11   | hurd           | https://github.com/Unicorn-Forest/hurd | `external/unicorn-forest-repos/hurd` |
| #12   | libpthread     | https://github.com/Unicorn-Forest/libpthread | `external/unicorn-forest-repos/libpthread` |
| #13   | incubator      | https://github.com/Unicorn-Forest/incubator | `external/unicorn-forest-repos/incubator` |
| #14   | mig            | https://github.com/Unicorn-Forest/mig | `external/unicorn-forest-repos/mig` |
| #15   | procfs         | https://github.com/Unicorn-Forest/procfs | `external/unicorn-forest-repos/procfs` |
| #16   | unionfs        | https://github.com/Unicorn-Forest/unionfs | `external/unicorn-forest-repos/unionfs` |
| #17   | viengoos       | https://github.com/Unicorn-Forest/viengoos | `external/unicorn-forest-repos/viengoos` |
| #18   | web            | https://github.com/Unicorn-Forest/web | `external/unicorn-forest-repos/web` |
| #19   | glibc          | https://github.com/Unicorn-Forest/glibc | `external/unicorn-forest-repos/glibc` |

## Additional Repositories

The following repositories were also found in the Unicorn-Forest organization:

| Repository | URL | Local Path |
|------------|-----|------------|
| bash       | https://github.com/Unicorn-Forest/bash | `external/unicorn-forest-repos/bash` |
| h          | https://github.com/Unicorn-Forest/h | `external/unicorn-forest-repos/h` |

## Cloning Instructions

### Method 1: Clone All Repositories
```bash
./clone-repos.sh
```
This will clone both GNU Hurd repositories and Unicorn-Forest repositories.

### Method 2: Clone Only Unicorn-Forest Repositories
```bash
./clone-unicorn-forest.sh
```
This will clone only the Unicorn-Forest repositories.

## Repository Status

After cloning, check the status file:
```bash
cat external/UNICORN_FOREST_STATUS.txt
```

## Integration with Existing System

The Unicorn-Forest repositories are cloned alongside the existing GNU Hurd repositories:

- **GNU repositories**: `external/gnu-repos/` and `external/hurd-repos/`
- **Unicorn-Forest repositories**: `external/unicorn-forest-repos/`

This allows for parallel development and comparison between the official GNU repositories and the Unicorn-Forest variants.