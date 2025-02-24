#! /bin/bash
set -e

# Run shelltestrunner tests.
# Translation artifacts will be cleaned always before running tests,
# and after *only if the testuite succeded* (to ease debugging).
# If you want to clean atifacts manually, type `./run-tests.sh clean`.

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

if [ "$1" = "clean" ]; then
  cleanArtifacts
  exit
fi

# Build and fetch proper binary.
cabal build hgettext
Bin=$(cabal list-bin hgettext)


# Run all tests.
cleanArtifacts
shelltest --color --execdir --with "$Bin" test/shelltest/
cleanArtifacts
