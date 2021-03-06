# configure.sh --
#
# Run this to configure.

set -xe

prefix=/usr/local
if test -d /lib64
then libdir=${prefix}/lib64
else libdir=${prefix}/lib
fi

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="${prefix}"                        \
    --libdir="${libdir}"			\
    --enable-time-tests				\
    CFLAGS='-O3'				\
    VFLAGS='-O3'				\
    "$@"

#    --with-nausicaa

### end of file
