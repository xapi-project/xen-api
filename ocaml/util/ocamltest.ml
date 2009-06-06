(* A unit testing framework for OCaml.                        *)
(* Author: Jonathan Knowles                                   *)
(* Copyright: 2008 Citrix Systems Research & Development Ltd. *)

open Printf

(* === Types === *)

type test =
	| Case  of name * description * case
	| Suite of name * description * suite
	and name        = string
	and description = string
	and case        = unit -> unit
	and suite       = test list

exception Failure_expected
exception Fail of string
exception Skip of string

(* === Equality assertions === *)

let assert_equal ?to_string x y =
	if not (x = y) then raise
	(Fail
		(match to_string with
			| None ->
				"found different values where equal values were expected."
			| Some to_string -> sprintf
				"found different values where equal values were expected: %s != %s."
				(to_string x) (to_string y)
		)
	)

let assert_equal_bool   = assert_equal ~to_string:string_of_bool
let assert_equal_float  = assert_equal ~to_string:string_of_float
let assert_equal_int    = assert_equal ~to_string:string_of_int
let assert_equal_int32  = assert_equal ~to_string:Int32.to_string
let assert_equal_int64  = assert_equal ~to_string:Int64.to_string
let assert_equal_string = assert_equal ~to_string:(fun s -> s)

(* === Other assertions === *)

let successful fn = try fn (); true with _ -> false

let assert_true x = assert x

let assert_false x = assert (not x)

let assert_raises_match exception_match fn =
	try
		fn ();
		raise Failure_expected
	with failure ->
		if not (exception_match failure)
			then raise failure
			else ()

let assert_raises expected =
	assert_raises_match (function exn -> exn = expected)

let assert_raises_any f =
	try
		f ();
		raise Failure_expected
	with failure ->
		()

let fail message = raise (Fail (message))

let skip message = raise (Skip (message))

(* === Console styles === *)

type style = Reset | Bold | Reverse | Dim | Red | Green | Blue | Yellow | Black

let int_of_style = function
	| Reset  ->  0
	| Bold   ->  1
	| Dim    ->  2
	| Reverse->  7
	| Black  -> 30
	| Red    -> 31
	| Green  -> 32
	| Yellow -> 33
	| Blue   -> 34

let string_of_style value = string_of_int (int_of_style value)

let escape = String.make 1 (char_of_int 0x1b)

(** Whether or not to disable pretty-printing. *)
let ugly = ref false

let style values =
	if !ugly then "" else
	sprintf "%s[%sm" escape (String.concat ";" (List.map (string_of_style) values))

(* === Indices === *)

let index_of_test =
	let rec build prefix = function
		| Case (name, description, case) ->
			[(prefix ^ name, Case (name, description, case))]
		| Suite (name, description, tests) ->
			(prefix ^ name, Suite (name, description, tests)) ::
			(List.flatten (List.map (build (prefix ^ name ^ ".")) tests))
	in
	build ""

let string_of_index_entry = function
	| (key, Case  (_, description, _))
	| (key, Suite (_, description, _))
	-> (style [Bold]) ^ key ^ (style [Reset]) ^ "\n    " ^ description

let string_of_index index =
	"\n" ^ (String.concat "\n" (List.map string_of_index_entry index)) ^ "\n"

let max x y = if x > y then x else y

let longest_key_of_index index =
	List.fold_left
		(fun longest_key (key, _) ->
			max longest_key (String.length key))
		0 index

(* === Runners === *)

type test_result = {passed: int; failed: int; skipped: int}

let add_result
	{passed = p1     ; failed = f1     ; skipped = s1     }
	{passed =      p2; failed =      f2; skipped =      s2} =
	{passed = p1 + p2; failed = f1 + f2; skipped = s1 + s2}

let singleton_pass = {passed = 1; failed = 0; skipped = 0}
let singleton_fail = {passed = 0; failed = 1; skipped = 0}
let singleton_skip = {passed = 0; failed = 0; skipped = 1}

(** True if (and only if) the currently-executing  *)
(** test has generated one or more debug messages. *)
let debugging = ref false

let start_debugging () =
	if not !debugging then
	begin
		debugging := true;
		print_endline "\n"
	end

(** Runs the given test. *)
let run test =

	let longest_key_width = longest_key_of_index (index_of_test test) in

	(** Runs the given test with the given name prefix. *)
	let rec run (test : test) (name_prefix : string) : test_result =
		match test with
			| Case (name, description, fn) ->
				run_case (name_prefix ^ name, description, fn)
			| Suite (name, description, tests) ->
				run_suite (name_prefix ^ name, description, tests)

	(** Runs the given test case. *)
	and run_case (name, description, fn) =

		let pre_status_padding =
			String.make (longest_key_width - (String.length name)) ' ' in

		let generate_status_string colour result =
			sprintf "%s\t[%s%s%s]" pre_status_padding (style [colour; Bold]) result (style [Reset])
		in

		let describe_current_test () =
			printf "%stesting %s%s" (style [Bold]) name (style [Reset]);
			flush stdout
		in

		let display_start_message () =
			describe_current_test ();
			debugging := false
		in

		let display_finish_message colour result =
			if !debugging
			then
				begin
					print_endline "";
					describe_current_test ();
				end;
			print_endline (generate_status_string colour result)
		in

		display_start_message ();
		try
			fn ();
			display_finish_message Green "pass";
			singleton_pass
		with
			| Skip (message) ->
				display_finish_message Blue "skip";
				printf "\nskipped: %s\n\n" message;
				singleton_skip
			| Fail (message) ->
				display_finish_message Red "fail";
				printf "\nfailed: %s\n\n" message;
				singleton_fail
			| failure ->
				display_finish_message Red "fail";
				printf "\nfailed: %s\n%s\n"
					(Printexc.to_string failure)
					(Printexc.get_backtrace ());
				singleton_fail

	(** Runs the given test suite. *)
	and run_suite (name, description, tests) =
		flush stdout;
		let result = List.fold_left (
			fun accumulating_result test ->
				add_result accumulating_result (run test (name ^ "."))
		) {passed = 0; failed = 0; skipped = 0} tests in
		result
	in

	Printexc.record_backtrace true;
	printf "\n";
	let {passed = passed; failed = failed; skipped = skipped} = run test "" in
	printf "\n";
	printf " tested [%s%i%s]\n" (style [Bold]) (passed + failed + skipped) (style [Reset]);
	printf " passed [%s%i%s]\n" (style [Bold]) (passed                   ) (style [Reset]);
	printf " failed [%s%i%s]\n" (style [Bold]) (         failed          ) (style [Reset]);
	printf "skipped [%s%i%s]\n" (style [Bold]) (                  skipped) (style [Reset]);
	printf "\n";
	{passed = passed; failed = failed; skipped = skipped}

let print_endline string =
	start_debugging ();
	print_endline string;
	flush stdout

let print_string string =
	start_debugging ();
	print_string string;
	flush stdout

(* === Factories === *)

let make_test_case name description case =
	Case (name, description, case)

let make_test_suite name description suite =
	Suite (name, description, suite)

let make_module_test_suite name suite =
	Suite (name, sprintf "Tests the %s module." name, suite)

(* === Command line interface === *)

(** Argument values. *)
let list = ref false
let name = ref None

(** Argument definitions. *)
let arguments =
[
	"-list",
		Arg.Set list,
		"lists the tests available in this module";
	"-name",
		Arg.String (fun name' -> name := Some name'),
		"runs the test with the given name";
	"-ugly",
		Arg.Set ugly,
		"disables pretty-printing";
]

(** For now, ignore anonymous arguments. *)
let process_anonymous_argument string = ()

(** For now, present a blank usage message. *)
let usage = ""

let make_command_line_interface test =
	(* TODO: Use stderr in appropriate places when presented with failures. *)
	Arg.parse arguments process_anonymous_argument usage;
	let index = index_of_test test in
	if !list
	then
		begin
			print_endline (string_of_index index);
			flush stdout
		end
	else
		begin
			let {passed = passed; failed = failed; skipped = skipped} = run
				(match !name with
					| Some name -> (List.assoc name index)
					| None -> test)
			in
			flush stdout;
			exit (if failed = 0 then 0 else 1)
		end
