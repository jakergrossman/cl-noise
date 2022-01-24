#/bin/sh

# BUILD MULTI-EXECUTABLES IN `BUILD_DIR'
#
# WHEN `MULTI_EXE_ONLY' IS SET, ONLY BUILD
# MULTI-EXECUTABLES, BUT DO NOT CREATE LOCAL
# SYMLINKS

# TODO: better build system coordination wrt multi-executable setup
#  - which exe is primary? which are links?

set -e

BUILD_DIR=${BUILD_DIR-bin}

build_multi_exe () {
    SYSTEM=$1
    MULTI_EXE=$2 # pick first executable name for multi-executable
    shift 2
    LINKS="$@"

    echo ":INFO: Building '$MULTI_EXE' multi-executable"
    sbcl --noinform \
         --non-interactive \
         --disable-debugger \
         --eval "(require 'asdf)" \
         --eval "(push \"$(dirname $0)/\" asdf:*central-registry*)" \
         --eval "(asdf:make \"noise/$SYSTEM\")"

    if [ -z "$MULTI_EXE_ONLY" ]; then
        for exe in $LINKS; do
            FILE="$BUILD_DIR/$exe"
            echo ":INFO:    Generating '$exe' executable"
            rm -rf "$BUILD_DIR/$exe"
            ln -sf $(realpath "$BUILD_DIR/$MULTI_EXE") $(realpath "$BUILD_DIR/$exe")
        done
    fi
}

build_multi_exe txt lnoise lperlin
build_multi_exe png lnoise-png lperlin-png
