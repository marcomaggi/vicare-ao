dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/AO],[0.4d0],[marco.maggi-ipsu@poste.it],
  [vicare-ao],[http://marcomaggi.github.com/vicare.html])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign subdir-objects dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CC_C99
AC_PROG_CC_C_O
AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
LT_PREREQ([2.4])
LT_INIT([shared disable-static])

VICARE_SCHEME
VICARE_CHECK_LIBRARY([VICARE],
  [(vicare (or (0 4 2015 5 (>= 23))
               (0 4 2015 (>= 6))
               (0 4 (>= 2016))))])
AS_IF([test "$vicare_cv_schemelib_VICARE" = no],
  [AC_MSG_ERROR([wrong Vicare version],1)])

VICARE_CHECK_LIBRARY([VICARE_FFI],
  [(vicare ffi (or (0 4 2015 5 (>= 28))
                   (0 4 2015 (>= 6))
                   (0 4 (>= 2016))))])
AS_IF([test "$vicare_cv_schemelib_VICARE_FFI" = no],
  [AC_MSG_ERROR([wrong Vicare FFI library version],1)])

# Command  line  options  for  the executable  "vicare".   The  variable
# AX_VFLAGS is  for options preselected by  the building infrastructure.
# The variable VFLAGS  is for user options selected on  the command line
# of "configure" and "make", for example:
#
#    $ make VFLAGS="-g --print-loaded-libraries"
#
AS_VAR_SET(AX_VFLAGS,["-O2 --verbose"])
AS_VAR_SET_IF(VFLAGS,,[AS_VAR_SET(VFLAGS)])

VICARE_OPTION_DEBUGGING_MODE
dnl VICARE_OPTION_NAUSICAA([check])
dnl AM_CONDITIONAL([WANT_NAUSICAA],[test x$vicare_with_nausicaa = xyes])

AM_CONDITIONAL([WANT_VICARE_DEBUG],[test x$VICARE_DEBUG = xyes])
AM_COND_IF([WANT_VICARE_DEBUG],[AS_VAR_SET(VFLAGS,"$VFLAGS -g")])

VICARE_ENABLE_OPTION([INSTALL_SOURCES],[sources-installation],[no],
  [whether to install source Scheme libraries],
  [enable installation of source Scheme libraries])
AM_CONDITIONAL([WANT_INSTALL_SOURCES],
  [test "x$vicare_enable_INSTALL_SOURCES" = xyes])

AC_LANG([C])
AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

VICARE_ENABLE_OPTION([TIME_TESTS],[time-tests],[no],
  [whether to enable timing of tests],
  [enable timing of tests])
AM_CONDITIONAL([WANT_TIME_TESTS],[test "x$vicare_enable_TIME_TESTS" = xyes])

AM_COND_IF([WANT_TIME_TESTS],
  [AC_PATH_PROG([TIME_PROGRAM],[time])])

#page
#### ao configuration

PKG_CHECK_MODULES([AO],[ao >= 1.2.0],[ao=yes],[ao=no])

LIBS="$AO_LIBS $LIBS"
CFLAGS="$AO_CFLAGS $CFLAGS"

AS_IF([test "x$ao" = "xyes"],
  [AC_DEFINE([HAVE_AO_H],[1],[true if AO header file is present])
   CPPFLAGS="$LIBS $AO_CPPFLAGS"
   LIBS="$LIBS $AO_LDFLAGS"],
  [AC_MSG_FAILURE([unable to find required version of AO],[1])])

VICARE_EXTENSION_LIBRARY_VERSION([ao],[0],[0],[0])

#page
#### C language preprocessor symbols from the file "libesmtp.h"

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_AO_H
#  include <ao/ao.h>
#endif
])

VICARE_CONSTANT_TESTS([AO_TYPE_LIVE
    AO_TYPE_FILE
    AO_ENODRIVER
    AO_ENOTFILE
    AO_ENOTLIVE
    AO_EBADOPTION
    AO_EOPENDEVICE
    AO_EOPENFILE
    AO_EFILEEXISTS
    AO_EBADFORMAT
    AO_EFAIL
    AO_FMT_LITTLE
    AO_FMT_BIG
    AO_FMT_NATIVE])

#page
#### C language functions from the header file "libesmtp.h"

## library setup/teardown
AC_CHECK_FUNCS([ao_initialize ao_shutdown])

## device setup/playback/teardown
AC_CHECK_FUNCS([ao_append_global_option ao_append_option ao_free_options ao_open_live ao_open_file ao_play ao_close])

## driver information
AC_CHECK_FUNCS([ao_driver_id ao_default_driver_id ao_driver_info ao_driver_info_list ao_file_extension])

## miscellaneous
AC_CHECK_FUNCS([ao_is_big_endian])

#page
#### finish

VICARE_OUTPUT
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
   [lib/vicare/multimedia/ao/constants.vicare.sls])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
