#! /bin/bash
set -e

## TODO clean artifacts regardless

### Helpers

# Clean stale artifacts from previous runs.
function cleanArtifacts {
    echo "Cleaning artifacts…"
    find test/shelltest/ -type f \
        -regextype posix-egrep -regex ".*\.po" -delete
}


### Program

clear

if ! which shelltest > /dev/null; then
  echo "shelltest not installed, install it (shelltestrunner)!"
  exit 1
fi

cabal build hgettext
Bin=$(cabal list-bin hgettext)


# Run all tests, and clean before (always) and after (only if shelltest
# exits without errors).
cleanArtifacts
shelltest --color --execdir --with "$Bin" test/shelltest/
cleanArtifacts
