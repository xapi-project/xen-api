type t = { name: string;
	   parts: part list }

and part = 
  | Hole of string
  | String of string

let string_of substitutions t = 
  let assoc x substitutions = 
    try List.assoc x substitutions
    with Not_found ->
      failwith (Printf.sprintf "Couldn't find substitution [%s] in template %s"
		  x t.name) in

  let stuff = List.map (function 
			| Hole x -> assoc x substitutions
			| String x -> x) t.parts in
  String.concat "" stuff

let output_fmt ff t = 
  let part = function
    | Hole x -> Format.fprintf ff "<[=%s]>" x
    | String x -> Format.fprintf ff "%s" x in
  Format.fprintf ff "@[ <[ %s ]>" t.name;
  List.iter part t.parts;
  Format.fprintf ff "<[ end ]> @]"
