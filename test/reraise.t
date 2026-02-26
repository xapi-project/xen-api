  $ ./raiser.exe -no-backtraces
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  raiser.exe: Thread 0 has no backtrace table

  $ ./raiser.exe -reraise
  Backtrace lab failed with exception Failure("bar")
  Raised Failure("bar")
  1/2 raiser.exe Raised at file lib/backtrace.ml, line 250
  2/2 raiser.exe Called from file test/log.ml, line 55
  

  $ ./raiser.exe -v1-with-backtrace
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  raiser.exe: Thread 0 has no backtrace table
