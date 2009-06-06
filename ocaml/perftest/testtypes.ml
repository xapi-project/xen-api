
(* Test results *)

type resultdata = 
    | StartTest of float list
    | SizeTest of float
    | ShutdownTest of float list
    | CloneTest of float list (* one float list per gold VM cloned *)

type result = {
  resultname : string;
  subtest : string;
  xenrtresult : float;
  rawresult : resultdata; (* Specific to the actual test *)
}

let header = "RAW"

let sep = ':'

let to_string (results:result list) =
	Printf.sprintf "%s%c%s" header sep (Marshal.to_string results [Marshal.No_sharing])

let from_string s : result list option =
	if Stringext.String.startswith header s
	then begin
		match Stringext.String.split ~limit:2 sep s with
			| [_; r] -> Some (Marshal.from_string r 0)
			| _ -> None
	end else
		None
