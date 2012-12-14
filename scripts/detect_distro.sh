#!/bin/bash

grep -i debian /etc/issue > /dev/null && echo Debianlike
grep -i ubuntu /etc/issue > /dev/null && echo Debianlike
grep -i centos /etc/issue > /dev/null && echo Centoslike
grep -i redhat /etc/issue > /dev/null && echo Centoslike
