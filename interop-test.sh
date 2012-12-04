#!/bin/sh

rm -rf /tmp/link_test
mkdir /tmp/link_test
./_build/core_test/link_test_main.native
PYTHONPATH=core python ./core_test/message_switch_test.py
