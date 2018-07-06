
module Interface(R : Idl.RPC) = struct
  open R

  let int_p_named_1 = Idl.Param.mk ~name:"int1" ~description:["first int param"] Rpc.Types.int
  let int_p_named_2 = Idl.Param.mk ~name:"int2" ~description:["second int param"] Rpc.Types.int
  let int_p_result = Idl.Param.mk ~name:"int" ~description:["int result"] Rpc.Types.int

  let add = R.declare "add"
      ["Add two numbers"]
      (int_p_named_1 @-> int_p_named_2 @-> returning int_p_result Idl.DefaultError.err)

  let bool_p_named_1 = Idl.Param.mk ~name:"bool1" ~description:["first bool param"] Rpc.Types.bool
  let bool_p_named_2 = Idl.Param.mk ~name:"bool2" ~description:["second bool param"] Rpc.Types.bool
  let bool_p_result = Idl.Param.mk ~description:["bool result"] Rpc.Types.bool

  let _land = R.declare "land"
      ["Logical and"]
      (bool_p_named_1 @-> bool_p_named_2 @-> returning bool_p_result Idl.DefaultError.err)

  let unit_p = Idl.Param.mk Rpc.Types.unit

  let noop = R.declare "noop"
      ["Do nothing"]
      (bool_p_named_1 @-> returning unit_p Idl.DefaultError.err)

  let implementation = implement
      { Idl.Interface.name = "Calc"; namespace = Some "Calc"; description = ["interface"]; version = (1,0,0) }
end

module IfCode = Interface(Codegen.Gen ())

let interfaces =
  Codegen.Interfaces.create
    ~name:"test_interface"
    ~title:"Test Interface"
    ~description:[{|Test Interface for integer arithmetic|}]
    ~interfaces:[IfCode.implementation ()]

let gen_python file =
  let oc = open_out file in
  output_string oc (Pythongen.of_interfaces interfaces |> Pythongen.string_of_ts);
  close_out oc

let run_linters file =
  let run msg cmd = Alcotest.(check int) msg 0 (Sys.command cmd) in
  run "pylint should exit with 0" ("pylint --errors-only " ^ file);
  run "pycodestyle should exit with 0" ("pycodestyle --ignore=E501 " ^ file)

let lint_bindings () =
  let file = "python/bindings.py" in
  gen_python file;
  run_linters file

let run ?input cmd =
  let inp, out = Unix.open_process cmd in
  begin match input with Some input -> output_string out input | None -> () end;
  close_out out;
  let l = input_line inp in
  close_in inp;
  l |> String.trim

let test_commandline () =
  gen_python "python/calc_impl/bindings.py";
  let run ?input cmd = run ?input ("python python/calc_impl/" ^ cmd) in
  let n = run "Calc.add 4 5" in
  Alcotest.(check string) "Calc.add with parameters passed on the command line" "9" n;
  let n = run ~input:{|{"int1":3,"int2":2}|} "Calc.add --json" in
  Alcotest.(check string) "Calc.add with parameters passed to stdin as JSON" "5" n;

  let b = run "Calc.land false true" in
  Alcotest.(check string) "Calc.land with parameters passed on the command line" "false" b;
  let b = run ~input:{|{"bool1":true,"bool2":true}|} "Calc.land --json" in
  Alcotest.(check string) "Calc.land with parameters passed to stdin as JSON" "true" b;

  let b = run "Calc.noop false" in
  Alcotest.(check string) "Calc.noop with parameters passed on the command line" "null" b;
  let b = run ~input:{|{"bool1":true}|} "Calc.noop --json" in
  Alcotest.(check string) "Calc.noop with parameters passed to stdin as JSON" "null" b

let check_test_class () =
  gen_python "python/calc_test/bindings.py";
  let run ?input cmd = run ?input ("python python/calc_test/" ^ cmd) in
  run "Calc.add 4 5" |> ignore;
  run ~input:{|{"int1":3,"int2":2}|} "Calc.add --json" |> ignore;
  run "Calc.land false true" |> ignore;
  run ~input:{|{"bool1":true,"bool2":true}|} "Calc.land --json" |> ignore;
  run "Calc.noop false" |> ignore;
  run ~input:{|{"bool1":true}|} "Calc.noop --json" |> ignore

let tests =
  [ "Check generated test interface bindings with pylint & pycodestyle", `Slow, lint_bindings
  ; "Check generated commandline bindings", `Slow, test_commandline
  ; "Check generated test class with commandline bindings", `Slow, check_test_class
  ]
