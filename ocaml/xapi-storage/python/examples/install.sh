#!/bin/bash

FILES=`find {datapath,volume} -type f -name \*.py`


for file in $FILES
do
    DIR=`dirname $file`

    mkdir -p /usr/libexec/xapi-storage-script/$DIR

    cp $file /usr/libexec/xapi-storage-script/$file
done

for file in $FILES
do
    BASE=`basename $file`
    DIR=`dirname $file`
    (
        cd /usr/libexec/xapi-storage-script/$DIR
        CMDS=`PYTHONPATH=$PYTHONPATH:/usr/libexec/xapi-storage-script/$DIR ./$BASE`
        if [ -n "$CMDS" ]
        then
            for cmd in $CMDS
            do
                ln -f $BASE $cmd
            done
        fi
    )

done
