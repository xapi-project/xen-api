open Types
open Files

let _ =
	let apis = [
		Smapiv2.api;
		Xenops.api;
		Memory.api;
		Clocksource.api;
	] in
	(* Prepend the debug_info argument *)
	let apis = List.map Types.prepend_dbg apis in

	Html.write apis;

	List.iter
		(fun api ->
			with_output_file (Printf.sprintf "python/%s.py" api.Interfaces.name)
				(fun oc ->
					let idents, api = resolve_refs_in_api api in
					output_string oc (Python.of_interfaces idents api |> Python.string_of_ts)
				)
		) apis;

	List.iter
		(fun api ->
			with_output_file (Printf.sprintf "ocaml/lib/%s.ml" api.Interfaces.name)
				(fun oc ->
					let idents, api = resolve_refs_in_api api in
					Ocaml.write_examples (Printf.sprintf "ocaml/examples/%s" api.Interfaces.name) idents api;
					output_string oc (Ocaml.of_interfaces idents api |> Ocaml.string_of_ts)
				)
		) apis

