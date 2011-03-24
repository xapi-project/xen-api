# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([xen-api-libs], [0.1], [xen-api@lists.xensource.com])
AC_CONFIG_SRCDIR([])
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_AUX_DIR(./mk)

AC_CANONICAL_SYSTEM
plat=generic
case $target_os in
darwin*)
  plat=darwin
  ;;
*)
  ;;
esac

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([string.h sys/param.h])
AC_CHECK_HEADERS([xen/xen.h],[XEN=1],[XEN=0])
AC_CHECK_HEADERS([libdevmapper.h], [DEVICE_MAPPER=1], [DEVICE_MAPPER=0])
AC_CHECK_HEADERS([linux/cdrom.h], [LINUX_CDROM=1], [LINUX_CDROM=0])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_DECLS([XS_RESTRICT],[],[],[
[
#include <errno.h>
#include <xenctrl.h>
#include <xen/io/xs_wire.h>
]])

# Checks for library functions.
AC_CHECK_FUNCS([realpath])

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG([xmlm])
AC_CHECK_OCAML_PKG([type-conv])

AC_CONFIG_FILES([Makefile config.mk])
AC_SUBST([XEN])
AC_SUBST([DEVICE_MAPPER])
AC_SUBST([LINUX_CDROM])
AC_SUBST([OCAML_PKG_xmlm])
AC_SUBST([OCAML_PKG_type_conv])
AC_OUTPUT
