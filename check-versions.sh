#!/bin/bash

# Check the version that installed programs report with --version.
#
# Usage: DESTDIR=<dir> ./check-versions.sh [-v] [-s], after
# make install DESTDIR=<dir>. make check-versions DESTDIR=<dir> [V=1] runs
# it with -s.
#
# The checks can only be done once the programs are installed: they also
# verify where each program is installed, and that none is left out.
#
# The correct version is the one given to ./configure --xapi_version,
# without its leading "v". What really matters is that every program gets
# its version from its package; a configured version is what makes this
# observable, since without one dune falls back to git describe for every
# program, attached to a package or not.
#
# A program that links the Xapi_version module, where the configured
# version is defined, must use that version, and no other, wherever it
# needs its own version. This script checks the version the program
# reports with --version. Every such program installed under DESTDIR is
# listed below, once, with the check describing what the program does
# today, right or wrong. A program that behaves differently, even better,
# makes the check fail: the commit that changes a behaviour updates the
# check of the program. The programs that do not link Xapi_version are
# not checked.
#
# The checks are:
#
# check_version
#   the program reports the correct version.
#
# By default, only failures are reported, on stderr. With -v, successes are
# reported too, on stdout. With -s, a summary is printed last, on stdout:
# the number of programs linking Xapi_version found under DESTDIR, the
# number of checks
# requested and, for each check, the number of programs that passed it,
# then the number of programs that failed their check.

set -u

usage () {
  echo "Usage: DESTDIR=<dir> $0 [-v] [-s]" 1>&2
  exit 2
}

VERBOSE=false
SUMMARY=false
for arg in "$@"; do
  case "$arg" in
    -v) VERBOSE=true ;;
    -s) SUMMARY=true ;;
    *) usage ;;
  esac
done

DESTDIR=${DESTDIR:-}
if [ -z "$DESTDIR" ]; then
  echo "DESTDIR is not set: it must be the directory given to" \
    "make install DESTDIR=<dir>" 1>&2
  exit 2
fi
if [ ! -d "$DESTDIR" ]; then
  echo "DESTDIR=$DESTDIR is not an existing directory: it must be the" \
    "directory given to make install DESTDIR=<dir>" 1>&2
  exit 2
fi
DESTDIR=${DESTDIR%/}

SRCDIR=$(cd "$(dirname "$0")" && pwd)
if [ ! -r "$SRCDIR/config.sh" ]; then
  echo "$SRCDIR/config.sh not found: run ./configure first" 1>&2
  exit 2
fi
# shellcheck source=/dev/null
. "$SRCDIR/config.sh"

if [ -z "$XAPI_VERSION" ]; then
  echo "No configured version. To configure one, use a command like:" 1>&2
  echo "  ./configure --xapi_version=v1912.6.23" 1>&2
  exit 2
fi
CORRECT_VERSION=${XAPI_VERSION#v}

FAILURES=0
REQUESTED=0
FOUND=0
declare -A RECORDED=()
declare -A PASSED=()

# Reporting

verbose () {
  if $VERBOSE; then
    echo "$@"
  fi
}

# block LABEL TEXT: print TEXT as a block of lines, or (empty)
block () {
  if [ -z "$2" ]; then
    echo "  $1: (empty)"
  else
    echo "  $1:"
    printf '%s\n' "$2" | sed 's/^/  | /'
  fi
}

# pass PROGRAM PROPERTY: report a success, counted for the calling check
pass () {
  PASSED[${FUNCNAME[1]}]=$(( ${PASSED[${FUNCNAME[1]}]:-0} + 1 ))
  verbose "checking whether $1 $2... yes"
}

# fail PROGRAM PROPERTY [DETAIL...]: the details are lines printed as such
fail () {
  {
    echo "checking whether $1 $2... no"
    shift 2
    for line in "$@"; do
      echo "$line"
    done
  } 1>&2
  FAILURES=$((FAILURES + 1))
}

# fail_run PROGRAM PROPERTY [EXPECTED...]: fail, showing what the program did
fail_run () {
  fail "$@" \
    "  actual exit status: $STATUS" \
    "$(block "actual stdout" "$OUT")" \
    "$(block "actual stderr" "$ERR")"
}

# Building blocks

# record PROGRAM: note that PROGRAM is checked, which must happen once
record () {
  REQUESTED=$((REQUESTED + 1))
  if [ -n "${RECORDED[$1]:-}" ]; then
    fail "$1" "is listed only once"
    return 1
  fi
  RECORDED[$1]=1
}

# installed PROGRAM: record PROGRAM and tell whether it is installed
installed () {
  record "$1" || return 1
  if [ -f "$DESTDIR$1" ] && [ -x "$DESTDIR$1" ]; then
    return 0
  fi
  fail "$1" "is installed" "  expected: an executable file at $DESTDIR$1"
  return 1
}

# run PROGRAM: run PROGRAM --version, setting OUT, ERR and STATUS. TERM is
# removed from the environment: on a host, most programs run without a
# terminal (services, cron jobs, programs started by xapi), and the result
# must not depend on where the script runs.
run () {
  local err
  err=$(mktemp)
  OUT=$(env -u TERM timeout 10 "$DESTDIR$1" --version 2>"$err")
  STATUS=$?
  ERR=$(cat "$err")
  rm -f "$err"
}

# Checks

check_version () {
  installed "$1" || return
  run "$1"
  local property="reports the correct version"
  if [ "$STATUS" -eq 0 ] && [ "$OUT" = "$CORRECT_VERSION" ] && [ -z "$ERR" ]
  then
    pass "$1" "$property"
  else
    fail_run "$1" "$property" \
      "  expected exit status: 0" \
      "$(block "expected stdout" "$CORRECT_VERSION")" \
      "  expected stderr: (empty)"
  fi
}

# check_all_checked: every installed program that links Xapi_version is
# listed below, and every program listed below links it. Programs are told
# from other files by their ELF header, and from shared libraries, which
# are ELF files too, by their .so extension; nm tells whether they link
# Xapi_version. Checking both ways guards against a change in the names
# that the compiler gives to symbols.
check_all_checked () {
  local file program unchecked="" undetected=""
  local -A detected=()
  while IFS= read -r -d '' file; do
    cmp -s -n 4 "$file" <(printf '\177ELF') || continue
    nm "$file" 2>/dev/null | grep -q camlXapi_version || continue
    FOUND=$((FOUND + 1))
    program=${file#"$DESTDIR"}
    detected[$program]=1
    if [ -z "${RECORDED[$program]:-}" ]; then
      unchecked+="$program"$'\n'
    fi
  done < <(find "$DESTDIR" -type f ! -name '*.so' -print0)
  for program in "${!RECORDED[@]}"; do
    if [ -z "${detected[$program]:-}" ]; then
      undetected+="$program"$'\n'
    fi
  done
  local property="checks every installed program that links Xapi_version"
  if [ -z "$unchecked" ] && [ -z "$undetected" ]; then
    pass "this script" "$property"
  else
    local details=()
    if [ -n "$unchecked" ]; then
      details+=("$(block "installed programs linking Xapi_version not listed" \
        "$(sort <<<"${unchecked%$'\n'}")")")
    fi
    if [ -n "$undetected" ]; then
      details+=("$(block "listed programs not detected as linking Xapi_version" \
        "$(sort <<<"${undetected%$'\n'}")")")
    fi
    fail "this script" "$property" "${details[@]}"
  fi
}

# count N SINGULAR PLURAL: print N followed by the form agreeing with it
count () {
  if [ "$1" -eq 1 ]; then
    echo "$1 $2"
  else
    echo "$1 $3"
  fi
}

# summary: print the summary, the checks in the order of the header
summary () {
  local passed=${PASSED[check_version]:-0}
  count "$FOUND" "program linking Xapi_version found under DESTDIR" \
    "programs linking Xapi_version found under DESTDIR"
  echo "$(count "$REQUESTED" "check requested" "checks requested"):"
  local n
  n=${PASSED[check_version]:-0}
  echo "  $(count "$n" "program reports" "programs report") the correct version ($CORRECT_VERSION)"
  n=$((REQUESTED - passed))
  echo "  $(count "$n" "program failed its check" "programs failed their check")"
}

verbose "correct version: $CORRECT_VERSION (from ./configure --xapi_version)"

# The programs, by installed path. The paths follow the install rules of the
# Makefile.
check_version                   "$OPTDIR/bin/mpathalert"
check_version                   "$OPTDIR/bin/rrd2csv"
check_version                   "$OPTDIR/bin/xapi"
check_version                   "$OPTDIR/debug/event_listen"
check_version                   "$OPTDIR/debug/quicktestbin"
check_version                   "$OPTDIR/debug/suspend-image-viewer"
check_version                   "$OPTDIR/debug/vncproxy"
check_version                   "$OPTDIR/libexec/alert-certificate-check"
check_version                   "$OPTDIR/libexec/daily-license-check"
check_version                   "$PREFIX/bin/gen_lifecycle"
check_version                   "$PREFIX/bin/qcow-stream-tool"
check_version                   "$PREFIX/bin/vhd-tool"
check_version                   "$XENOPSD_LIBEXECDIR/pvs-proxy-ovs-setup"
check_version                   "$PREFIX/sbin/message-cli"
check_version                   "$PREFIX/sbin/sm-cli"
check_version                   "$PREFIX/sbin/squeezed"
check_version                   "$PREFIX/sbin/varstored-guard"
check_version                   "$PREFIX/sbin/xapi-nbd"
check_version                   "$PREFIX/sbin/xapi-storage-script"
check_version                   "$PREFIX/sbin/xcp-networkd"
check_version                   "$PREFIX/sbin/xcp-rrdd"
check_version                   "$PREFIX/sbin/xenops-cli"
check_version                   "$PREFIX/sbin/xenopsd-simulator"
check_version                   "$PREFIX/sbin/xenopsd-xc"

check_all_checked

if [ "$FAILURES" -ne 0 ]; then
  echo "$FAILURES check(s) failed" 1>&2
fi
if $SUMMARY; then
  summary
fi
[ "$FAILURES" -eq 0 ]
