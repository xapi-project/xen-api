
module D=Debug.Debugger(struct let name="xapi" end)
open D


let check_for_bad_link () =
	(* Look for the exception catching bug caused by dodgy linking (thanks, ocamlfind) *)
	try
	  Unix.access "/opt/xensource/bin/doesntexist" [ Unix.F_OK ]
	with 
	| Unix.Unix_error(_, _, _) -> debug "Binary appears to be correctly linked"
	| e -> 
	    let msg = "This binary is broken: check your link lines (see c/s 4200:694e7dabb159)" in
	    debug "%s" msg;
	    failwith msg


