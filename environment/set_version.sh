#!/bin/sh

# exit if git isn't installed
if ! command -v git >/dev/null 2>&1; then exit; fi
# exit if we aren't in a git repository
if ! git rev-parse; then exit; fi

# Use git describe output with all - replaced with . and leading v removed.
VERSION=`git describe --abbrev=3 --tags HEAD \
        | sed -e "s/-/./g" -e "s/^v\(.*\)/\1/"`

# extract the current version definition
HEADER_VERSION=`sed -n 's/#define HALITE_VERSION "\([^"]*\)"/\1/p' version.hpp`

# Only update the header if the version has changed.
if [ "$VERSION" != "$HEADER_VERSION" ] ; then
    sed -i.bak "s/\(#define HALITE_VERSION \"\)[^\"]*\"/\1$VERSION\"/" version.hpp
    rm version.hpp.bak
fi
