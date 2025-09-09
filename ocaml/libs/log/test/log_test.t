The log_test executable produces a backtrace on purpose, on x86_64, and with
the datetimes removed, it looks like this:
$ ./log_test.exe | sed -re 's/[0-9]+T[0-9:.]+Z//'
[|error||0 |main|backtrace] Raised Invalid_argument("index out of bounds")
[|error||0 |main|backtrace] 1/4 log_test.exe Raised at file ocaml/libs/log/test/log_test.ml, line 7
[|error||0 |main|backtrace] 2/4 log_test.exe Called from file fun.ml, line 33
[|error||0 |main|backtrace] 3/4 log_test.exe Called from file fun.ml, line 38
[|error||0 |main|backtrace] 4/4 log_test.exe Called from file ocaml/libs/log/test/log_test.ml, line 18
[|error||0 |main|backtrace] 
[| warn||0 |main|log_test.ml] Got exception: Invalid_argument("index out of bounds")

and on aarch64:
[|error||0 |main|backtrace] Raised Invalid_argument("index out of bounds")
[|error||0 |main|backtrace] 1/3 log_test.exe Raised at file ocaml/libs/log/test/log_test.ml, line 7
[|error||0 |main|backtrace] 2/3 log_test.exe Called from file fun.ml, line 38
[|error||0 |main|backtrace] 3/3 log_test.exe Called from file ocaml/libs/log/test/log_test.ml, line 18
[|error||0 |main|backtrace] 
[| warn||0 |main|log_test.ml] Got exception: Invalid_argument("index out of bounds")

  $ ./log_test.exe | grep "main|backtrace" -c | xargs -I _ sh -c "test 5 -eq _ || test 6 -eq _"
  $ ./log_test.exe | grep "log_test.exe" -c | xargs -I _ sh -c "test 3 -eq _ || test 4 -eq _"
  $ ./log_test.exe | grep "ocaml/libs/log/test/log_test.ml" -c
  2
