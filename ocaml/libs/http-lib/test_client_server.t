== Bring server up
  $ trap 'kill $(jobs -p)' EXIT
  $ ./test_server.exe &
  $ sleep 0.1

== Normal
  $ ./test_client.exe --perf > /dev/null
