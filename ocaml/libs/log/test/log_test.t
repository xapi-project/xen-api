  $ ./log_test.exe | sed -re 's/[0-9]+T[0-9:.]+Z//'
  [|error||0 |main|backtrace] Raised Invalid_argument("index out of bounds")
  [|error||0 |main|backtrace] 1/4 log_test.exe Raised at file ocaml/libs/log/test/log_test.ml, line 7
  [|error||0 |main|backtrace] 2/4 log_test.exe Called from file ocaml/libs/xapi-stdext/lib/xapi-stdext-pervasives/pervasiveext.ml, line 24
  [|error||0 |main|backtrace] 3/4 log_test.exe Called from file ocaml/libs/xapi-stdext/lib/xapi-stdext-pervasives/pervasiveext.ml, line 39
  [|error||0 |main|backtrace] 4/4 log_test.exe Called from file ocaml/libs/log/test/log_test.ml, line 14
  [|error||0 |main|backtrace] 
  [| warn||0 |main|log_test.ml] Got exception: Invalid_argument("index out of bounds")

