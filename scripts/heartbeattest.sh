#!/bin/bash

if test $# -lt 2
then
    echo "Usage: heartbeattest.sh <options> <action> <master ip> <slaves ips>"
    echo "where options can be:"
    echo "  -r <sec>    random waittime between 0 and <sec>"
    echo "  -w <sec>    waittime"
    echo "  -t <sec>    total execution time"
    echo ""
    echo "where action can be:"
    echo "  master-vif-unplug"
    echo ""
    exit 1
fi

WAIT=120
TOTAL=0
while :
do
    if test $1 = "-w"
    then
        WAIT=$2
        shift
        shift
    elif test $1 = "-t"
    then
        TOTAL=$2
        shift
        shift
    elif test $1 = "-r"
    then
        R=$2
        shift
        shift
    else
        break
    fi
done

ACTION=$1
if test $ACTION != "master-vif-unplug"
then
    echo "action not supported!"
    exit 1
fi
shift

IP=()
UUID=()
OUTPUT=()
VIF=()
for i in $*
do
    IP=(${IP[@]} $i)
    TEMP=`xe vm-list params=networks,uuid | grep -B 1 $i | head -n 1 | awk '{print$5}'`
    UUID=(${UUID[@]} $TEMP)
    VIF=(${VIF[@]} `xe vif-list params=uuid vm-uuid=$TEMP --minimal`)
    echo "writing $i.log"
    echo "writing $i.log" > $i.log
done

COUNT=0
while test $COUNT -lt $TOTAL -o $TOTAL -eq 0
do
    echo "master vif unplug"
    xe vif-unplug uuid=${VIF[0]}
    
    if test $R
    then
        WAIT=$RANDOM
        let "WAIT %= $R"
    fi
    W1=$WAIT
    echo "sleeping $WAIT secs"
    sleep $WAIT
    COUNT=$(($COUNT + $WAIT))
    
    echo "master vif plug"
    xe vif-plug uuid=${VIF[0]}
    
    if test $R
    then
        WAIT=$RANDOM
        let "WAIT %= $R"
    fi
    
    W2=$WAIT
    echo "sleeping $WAIT secs"
    sleep $WAIT
    COUNT=$(($COUNT + $WAIT))
    
    for ((i=0; i < ${#IP[@]}; i++))
    do
        echo "master vif unplug" >> ${IP[$i]}.log
        echo "sleeping $W1 secs" >> ${IP[$i]}.log
        echo "master vif unplug" >> ${IP[$i]}.log
        echo "sleeping $W1 secs" >> ${IP[$i]}.log
        echo "RESULTS: `ssh root@${IP[$i]} ccm_tool -p`" >> ${IP[$i]}.log
    done
done

echo "done!"
