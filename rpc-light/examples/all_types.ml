type t = Foo of int | Bar of (int * float) with rpc

type x = {
	foo: t;
	bar: string;
	gna: float list;
	f1: (int option * bool list * float list list) option;
	f2: (string * string list) array;
	f3: int32;
	f4: int64;
 } with rpc

let _ =
	let x1 = {
		foo= Foo 3;
		bar= "foo";
		gna=[1.; 2.; 3.; 4. ];
		f2 = [| "hi",["hi"]; "hou",["hou";"hou"]; "foo", ["b";"a";"r"] |];
		f1 = None;
		f3 = Int32.max_int;
		f4 = Int64.max_int 
	} in
	let str = Xmlrpc.to_string (rpc_of_x x1) in
	Printf.printf "String: %s\n\n" str;
	let x2 = x_of_rpc (Xmlrpc.of_string str) in
	Printf.printf "Result: %s\nx1=x2: %b\n\n" str (x1 = x2)
