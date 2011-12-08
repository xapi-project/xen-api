#!/bin/bash
# Script that installs files and performs basic translation of text files.
# Citrix Systems Inc, 2011
#
# Usage: ./install.sh <mode> <file1> .. <filen> <dest>
#
# Replaces the following strings:
#
# @OPTDIR@ -> ${OPTDIR}
# @ETCDIR@ -> ${ETCDIR}
# @VARDIR@ -> ...etc...
# @VARPATCHDIR@
# @ETCDIR@
# @OPTDIR@
# @PLUGINDIR@
# @HOOKSDIR@
# @INVENTORY@
# @XAPICONF@
# @LIBEXECDIR@
# @SCRIPTSDIR@

set -x

MODE=${1}
NUM_FILES=$(($#-2))
FILES=${@:2:$NUM_FILES}
DEST=${!#}

INSTALL="install -m ${MODE}"

SCRIPTS_DIR=`dirname ${0}`
BASE_PATH=`${SCRIPTS_DIR}/base-path xapi.conf`

for FILE in ${FILES}; do
  ${INSTALL} ${FILE} ${DEST}
  if [ -d ${DEST} ]; then
    BASENAME=`basename ${FILE}`
    NEWFILE="${DEST}/${BASENAME}"
    NEWFILE=${NEWFILE//\/\//\/}
  else
    NEWFILE=${DEST}
  fi
  if file ${NEWFILE} | grep -q "text"; then
    sed -i -e "s!@OPTDIR@!${OPTDIR}!g" \
	-e "s!@ETCDIR@!${ETCDIR}!g" \
        -e "s!@VARDIR@!${VARDIR}!g" \
        -e "s!@VARPATCHDIR@!${VARPATCHDIR}!g" \
        -e "s!@PLUGINDIR@!${PLUGINDIR}!g" \
        -e "s!@HOOKSDIR@!${HOOKSDIR}!g" \
        -e "s!@INVENTORY@!${INVENTORY}!g" \
        -e "s!@XAPICONF@!${XAPICONF}!g" \
        -e "s!@LIBEXECDIR@!${LIBEXECDIR}!g" \
        -e "s!@SCRIPTSDIR@!${SCRIPTSDIR}!g" \
        -e "s!@SHAREDIR@!${SHAREDIR}!g" \
        -e "s!@WEBDIR@!${WEBDIR}!g" \
        -e "s!@XHADIR@!${XHADIR}!g" \
        -e "s!@BINDIR@!${BINDIR}!g" \
	-e "s!@SBINDIR@!${SBINDIR}!g" \
         ${NEWFILE}
  fi
done
