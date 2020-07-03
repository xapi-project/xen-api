#!/bin/bash

XC=libxenctrl

ldd "$1" | grep -q $XC 2>&1
if [ $? -eq 1 ]; then
	echo -e "\n\033[32;1m[OK]\033[0m $1 does not depend on $XC";
	exit 0
else
	echo -e "\n\033[31;1m[ERROR]\033[0m $1 depends on $XC";
	exit 1
fi
