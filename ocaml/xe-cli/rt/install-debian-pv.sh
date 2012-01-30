#!/bin/sh

set -e 

# Run this script on dom0 on the host that we're testing on (master or 
# slave, either is OK)

usage () {
        echo "Usage: $0 <network-device> [sr-uuid]"
        echo "Creates a VM suitable for use with test_host"
        echo "Network device should be e.g. xenbr0. SR specified must be attacha
ble to this host"
        exit 1
}

if [ -z "$1" ]; then
        usage
fi

NETWORKDEVICE=$1

if [ -z "$2" ]; then
        SRUUID=`xe pool-list params=default-SR --minimal`
else 
        SRUUID=$2
fi

# Get the host uuid for localhost
HOSTNAME=`hostname`
HOST=`xe host-list hostname=${HOSTNAME} --minimal`

# Check the SR is visible from here
NUMPBDS=`xe pbd-list sr-uuid=${SRUUID} host=${HOST} --minimal | wc -w`

if [ "${NUMPBDS}" == "0" ]; then
        echo "Cannot see requested storage from this host!"
        exit 1
fi

# Install the VM
VM=`xe vm-install template="Debian Etch 4.0" new-name-label=debian-pv sr-uuid=${SRUUID}`
xe vm-param-set uuid=${VM} PV-args=noninteractive

# Add the VIF
NETWORK=`xe network-list bridge=${NETWORKDEVICE} --minimal`
VIF=`xe vif-create vm-uuid=${VM} network-uuid=${NETWORK} device=0`

# We're going to fiddle with its disk, so find it:
VDI=`xe vbd-list vm-uuid=${VM} userdevice=0 params=vdi-uuid --minimal`

# Run the inner-script bit:
"@OPTDIR@/debug/with-vdi" ${VDI} "@OPTDIR@/debug/install-debian-pv-inside.sh"

# Start it up
xe vm-start vm=${VM}

# Wait for the VM to shut down, indicating that it's finished
xe event-wait class=vm uuid=${VM} power-state=halted

echo "VM is now set up for testing!"
