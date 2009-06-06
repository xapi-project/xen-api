#!/bin/bash

IP=$1
TESTHOST=$2

cp /usr/groups/xenrt/production/share/scripts/keys/id_dsa_xenrt /tmp/
chmod 600 /tmp/id_dsa_xenrt
ssh -i /tmp/id_dsa_xenrt root@$IP "mkdir /dev/fs/C/xen"
sed s/TESTHOST/$2/ <starttest_template.bat >starttest.bat
scp -i /tmp/id_dsa_xenrt starttest.bat "root@$IP:/dev/fs/C/Documents\ and\ Settings/Administrator/Start\ Menu/Programs/Startup/"
ssh -i /tmp/id_dsa_xenrt root@$IP "chmod 755 /dev/fs/C/Documents\ and\ Settings/Administrator/Start\ Menu/Programs/Startup/starttest.bat"
scp -i /tmp/id_dsa_xenrt wget.exe root@$IP:/dev/fs/C/
ssh -i /tmp/id_dsa_xenrt root@$IP "chmod 755 /dev/fs/C/wget.exe"
scp -i /tmp/id_dsa_xenrt nc.exe root@$IP:/dev/fs/C/
ssh -i /tmp/id_dsa_xenrt root@$IP "chmod 755 /dev/fs/C/nc.exe"
scp -i /tmp/id_dsa_xenrt ocamlrun.dll root@$IP:/dev/fs/C/xen/
scp -i /tmp/id_dsa_xenrt ocamlrun.exe root@$IP:/dev/fs/C/xen/
scp -i /tmp/id_dsa_xenrt dllunix.dll root@$IP:/dev/fs/C/xen/
ssh -i /tmp/id_dsa_xenrt root@$IP "chmod 755 /dev/fs/C/xen/*"
ssh -i /tmp/id_dsa_xenrt root@$IP "/dev/fs/C/WINDOWS/system32/netsh.exe firewall set opmode DISABLE"
