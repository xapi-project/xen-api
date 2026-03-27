  $ ./raiser.exe -no-backtraces
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  raiser.exe: Thread 0 has no backtrace table

  $ ./raiser.exe -reraise
  Backtrace lab failed with exception Failure("bar")
  Raised Failure("bar")
  1/4 raiser.exe Raised at Raiser.foo in file "ocaml/libs/backtrace/test/raiser.ml", line 1, characters 20-41
  2/4 raiser.exe Called from Raiser.bar in file "ocaml/libs/backtrace/test/raiser.ml", line 4, characters 6-12
  3/4 raiser.exe Re-raised at Backtrace.reraise in file "ocaml/libs/backtrace/lib/backtrace.ml", line 340, characters 2-14
  4/4 raiser.exe Called from Log.with_thread_associated in file "ocaml/libs/backtrace/test/log.ml", line 59, characters 6-9
  


  $ ./raiser.exe -v1-with-backtrace
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  raiser.exe: Thread 0 has no backtrace table

  $ ./raiser.exe -raise-again
  Backtrace lab failed with exception Failure("foo")
  Raised Failure("foo")
  1/4 raiser.exe Raised at Raiser.foo in file "ocaml/libs/backtrace/test/raiser.ml", line 1, characters 20-41
  2/4 raiser.exe Called from Raiser.baz in file "ocaml/libs/backtrace/test/raiser.ml", line 6, characters 17-23
  3/4 raiser.exe Re-raised at Raiser.baz in file "ocaml/libs/backtrace/test/raiser.ml", line 6, characters 36-45
  4/4 raiser.exe Called from Log.with_thread_associated in file "ocaml/libs/backtrace/test/log.ml", line 59, characters 6-9
  

