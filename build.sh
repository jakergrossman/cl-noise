#/bin/sh

set -e

case $1 in
    png|text)
        echo "[INFO] Generating executable \`$1'"
        sbcl --noinform \
             --non-interactive\
	     --disable-debugger \
             --eval "(require 'asdf)" \
             --eval "(push '*default-pathname-defaults* asdf:*central-registry*)" \
             --eval "(asdf:make \"cl-noise-$1/binary\")"
    ;;
    *)
        echo "[ERROR] No examples passed!"
        echo "[ERROR] Available:"
        echo "[ERROR]   png"
        echo "[ERROR]   text"
        ;;
esac
