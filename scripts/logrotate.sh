#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

exec 0</dev/null
exec /usr/sbin/logrotate /etc/logrotate.conf
