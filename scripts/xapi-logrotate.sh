#!/bin/sh

# If we're using the old partitioning scheme there's no need to run this.
if ! grep -q /var/log /proc/mounts; then
  exit 0
fi

/usr/sbin/logrotate /etc/xensource/xapi-logrotate.conf
EXITVALUE=$?
if [ $EXITVALUE != 0 ]; then
    /usr/bin/logger -t $0 "ALERT exited abnormally with [$EXITVALUE]"
fi
exit 0
