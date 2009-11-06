# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([xen-api-libs], [0.1], [xen-api@lists.xensource.com])
AC_CONFIG_SRCDIR([])
AC_CONFIG_HEADERS([stdext/config.h])
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

# Checks for typedefs, structures, and compiler characteristics.

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

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
