#/bin/sh

set -e

echo "Building 'noise'"
sbcl --noinform \
     --non-interactive \
     --disable-debugger \
     --eval "(push \"$(dirname $0)/\" asdf:*central-registry*)" \
     --eval "(asdf:make \"noise/binary\")"
