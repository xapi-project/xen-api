== Testing no-ops
  $ unset NOTIFY_SOCKET
  $ ./test_systemd.exe --notify
  NOTIFY_SOCKET not set, notification couldn't be sent
  [1]

== Use abstract sockets
  $ export NOTIFY_SOCKET="@systemd.socket"; echo "$NOTIFY_SOCKET"
  @systemd.socket
  $ ./test_systemd.exe --server &
  @systemd.socket
  READY=1
  $ sleep 1
  $ ./test_systemd.exe --notify
  $ wait

== Use socket files
  $ export TMPDIR=${TMPDIR:-/tmp}
  $ export NOTIFY_SOCKET="${TMPDIR}/systemd.socket"
  $ rm -f "$NOTIFY_SOCKET"
  $ ./test_systemd.exe --server &
  READY=1
  $ sleep 1
  $ test -S "$NOTIFY_SOCKET"
  $ ./test_systemd.exe --notify
  $ wait

== Currently not run tests because of insufficient permissions
==  in cram to be manipulating this file
$ mv /run/systemd/system /run/systemd/system.old
$ ./test_systemd.exe booted
Booted without systemd
$ mv /run/systemd/system.old /run/systemd/system
$ ./test_systemd.exe booted
Booted with systemd
