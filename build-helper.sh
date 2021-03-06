# -*- mode: bash;  -*-


CFLAGS="$CFLAGS $(/usr/bin/python3-config  --cflags)"
LDFLAGS="$LDFLAGS $(/usr/bin/python3-config   --ldflags)"

rm -f *.o *.so \
#   app

export libs="-lm"

# Note, the -Wl,-R flags will make our shared library available to the
# executable app from the location that it was compiled, rather than
# having to be installed globably or adding the build path to
# LD_LIBRARY_PATH.

export LDFLAGS="-L. -Wl,-R -Wl,. $LDFLAGS"
export CFLAGS="-DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall $CFLAGS"

gcc $CFLAGS -c app_main.c
gcc -shared -Wl,-soname,libapp_main.so $LDFLAGS -o libapp_main.so *o $libs
#gcc main.c $CFLAGS $LDFLAGS -lapp_main -lecl -o app
