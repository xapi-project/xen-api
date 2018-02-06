#!/bin/sh

../src/fe_main.exe &
for x in `seq 1 10`; do
	test -S /var/xapi/forker/main || sleep 1
done
./fe_test.exe 16
