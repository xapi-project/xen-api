  $ ./raiser.exe -no-backtraces
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  raiser.exe: Thread 0 has no backtrace table

  $ ./raiser.exe -reraise
  Backtrace lab failed with exception Failure("bar")
  Raised Failure("bar")
  1/4 raiser.exe Raised at file ocaml/libs/backtrace/test/raiser.ml, line 1
  2/4 raiser.exe Called from file ocaml/libs/backtrace/test/raiser.ml, line 4
  3/4 raiser.exe Called from file ocaml/libs/backtrace/lib/backtrace.ml, line 251
  4/4 raiser.exe Called from file ocaml/libs/backtrace/test/log.ml, line 55
  

  $ ./raiser.exe -v1-with-backtrace
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  raiser.exe: Thread 0 has no backtrace table

  $ ./raiser.exe -raise-again
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  1/4 raiser.exe Raised at file ocaml/libs/backtrace/test/raiser.ml, line 1
  2/4 raiser.exe Called from file ocaml/libs/backtrace/test/raiser.ml, line 6
  3/4 raiser.exe Called from file ocaml/libs/backtrace/test/raiser.ml, line 6
  4/4 raiser.exe Called from file ocaml/libs/backtrace/test/log.ml, line 55
  
