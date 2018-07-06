#!/bin/sh
set -ex

LINKPATH=/tmp/link_test

rm -rf ${LINKPATH} && mkdir -p ${LINKPATH}

lwt/link_test_main.exe
PYTHONPATH=core python message_switch_test.py
