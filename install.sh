#!/bin/sh

# COPY BUILT MULTI-EXECUTABLES FROM `BUILD_DIR'
# TO `PREFIX', CREATING SYMLINKS FOR ADDITIONAL
# EXECUTABLES

set -e

BUILD_DIR=${BUILD_DIR-bin}
PREFIX=${PREFIX-/usr/local/bin}

mkdir -p "$PREFIX"

for dest in lnoise lperlin lnoise-png lperlin-png; do
    if test -L "$PREFIX/$dest" || test -f "$PREFIX/$dest"; then
       echo "$PREFIX/$dest exists!"
       rm -i "$PREFIX/$dest"
    fi
done

cp "$BUILD_DIR"/lnoise "$PREFIX"
ln -sf $(realpath "$PREFIX"/lnoise) $(realpath "$PREFIX"/lperlin)

cp "$BUILD_DIR"/lnoise-png "$PREFIX"
ln -sf $(realpath "$PREFIX"/lnoise-png) $(realpath "$PREFIX"/lperlin-png)
