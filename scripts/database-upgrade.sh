#!/bin/bash
#
# Copyright (c) Citrix Systems 2008. All rights reserved.
#

echo Warning: this is a best-effort update attempt... old db will be in /var/xapi/state-old.db

sqlite3 /var/xapi/state.db .dump > /tmp/dump
mv /var/xapi/state.db /var/xapi/state-old.db
/opt/xensource/libexec/dbupdate -i /tmp/dump -d /etc/xensource/db-default-fields -o /tmp/newdump 
sqlite3 /var/xapi/state.db < /tmp/newdump
