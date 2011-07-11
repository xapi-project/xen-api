#!/bin/bash
# Script that installs files and performs basic translation of text files.
# Citrix Systems Inc, 2011
#
# Usage: ./install.sh <mode> <file1> .. <filen> <dest>
#
# Assumptions:
# - The 'base-path' script is located in the same directory.

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
    sed -i "s!<BASE_PATH>!${BASE_PATH}!g" ${NEWFILE}
  fi
done