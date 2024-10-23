== Bring server up
  $ trap 'kill $(jobs -p)' EXIT
  $ ./test_server.exe &
  $ sleep 0.1

== Normal
  $ ./test_client.exe --perf > /dev/null

== Expect to log after a closed connection
  $ ./test_client.exe --logerr > result
  $ grep "ECONNRESET" result -c
  1
  $ grep "backtrace" result -c
  11
  $ grep "Called from" result -c
  8
