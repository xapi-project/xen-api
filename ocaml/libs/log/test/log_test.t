  $ ./log_test.exe | sed -re 's/[0-9]+T[0-9:.]+Z//'
  [|debug||0 |main|log_test.ml] Raised at Xapi_stdext_pervasives__Pervasiveext.finally in file \"ocaml/libs/xapi-stdext/lib/xapi-stdext-pervasives/pervasiveext.ml\", line 39, characters 6-15\nCalled from Dune__exe__Log_test.(fun) in file \"ocaml/libs/log/test/log_test.ml\", line 14, characters 9-60\n
  [| warn||0 |main|log_test.ml] Got exception: Invalid_argument("index out of bounds")

