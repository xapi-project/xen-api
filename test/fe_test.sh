#!/bin/sh

PIDFILE=/var/xapi/forkexecd.pid

../src/fe_main.exe -daemon -pidfile $PIDFILE
trap 'kill `cat $PIDFILE`' EXIT

./fe_test.exe 16
