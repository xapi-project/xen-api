(* test the "." and ".." removal code *)

let table = [ 
  "/tmp/../../../.././././../", "/";
  "/tmp/foo/bar/../../", "/tmp";
  "/tmp/foo/bar/.././..", "/tmp";
  "/tmp/foo/bar/./././../../", "/tmp";
  "/tmp/foo/bar/../../../", "/"
]

let _ = 
  List.iter (fun (input, output) ->
	       let output' = Unixext.resolve_dot_and_dotdot input in
	       if output <> output'
	       then failwith (Printf.sprintf "input = [%s] output = [%s] expected = [%s]" input output' output)
	    ) table
