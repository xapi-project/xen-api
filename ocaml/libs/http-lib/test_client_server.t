== Bring server up
  $ trap 'kill $(jobs -p)' EXIT
  $ ./test_server.exe &
  $ sleep 1

== Normal
  $ ./test_client.exe > /dev/null
