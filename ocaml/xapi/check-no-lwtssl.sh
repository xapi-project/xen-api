#!/bin/bash

SSL=libssl
CRYPTO=libcrypto
# For now do not check for libev/Lwt,
# we need to fix ocaml-xenstore-clients and tar.unix first
DEPS="${SSL}|${CRYPTO}"

ldd "$1" | grep -q -E "${DEPS}" 2>&1
if [ $? -eq 1 ]; then
	echo -e "\n\033[32;1m[OK]\033[0m $1 does not depend on ${DEPS}";
	exit 0
else
	echo -e "\n\033[31;1m[ERROR]\033[0m $1 depends on ${DEPS}";
	exit 1
fi
