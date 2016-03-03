#!/bin/sh

PIDFILE=/var/xapi/forkexecd.pid

./fe_main.native -daemon -pidfile $PIDFILE
trap 'kill `cat $PIDFILE`' EXIT

./fe_test.native 16
