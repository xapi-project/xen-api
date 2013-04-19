#!/bin/bash

prefix=`opam config var prefix`


tmux new-session -d -s xapi

tmux new-window -t xapi:1 -n "xcp-fe" "sudo $prefix/bin/xcp-fe"
tmux new-window -t xapi:2 -n "xenopsd" "sudo $prefix/bin/xenopsd -config $prefix/etc/xenopsd.conf"
tmux new-window -t xapi:3 -n "xcp-networkd" "sudo $prefix/bin/xcp-networkd -daemon false"
tmux new-window -t xapi:4 -n "v6d" "sudo $prefix/lib/xcp/bin/v6d"
tmux new-window -t xapi:5 -n "xapi" "sudo $prefix/bin/xapi -nowatchdog -config $prefix/etc/xapi.conf"
tmux new-window -t xapi:6 -n "ffs" "sudo $prefix/bin/ffs"
tmux new-window -t xapi:6 -n "bash" "bash"

tmux select-window -t xapi:6
tmux -2 attach-session -t xapi

