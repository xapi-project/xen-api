open Cohttp_lwt_unix
open Lwt
open Protocol
open Protocol_lwt

let port = ref 8080

let main () =
	lwt c = IO.connect !port in
	let from = ref 0L in
	let timeout = 5. in
	let start = ref None in
	while_lwt true do
		match_lwt Connection.rpc c (In.Trace (!from, timeout)) with
		| Error e -> fail e
		| Ok raw ->
			let trace = Out.trace_of_rpc (Jsonrpc.of_string raw) in
			let endpoint = function
				| None -> "-"
				| Some x -> x in
			let message = function
				| Event.Message m -> m.Message.payload
				| Event.Ack id -> Printf.sprintf "ack %Ld" id in
			List.iter (fun (id, event) ->
				let time = match !start with
					| None ->
						start := Some event.Event.time;
						0.
					| Some t ->
						event.Event.time -. t in
				Printf.fprintf stdout "%Ld: %.1f: %10s -> %10s -> %10s: %s\n%!" id time
					(endpoint event.Event.input) event.Event.queue (endpoint event.Event.output)
					(message event.Event.message)
			) trace.Out.events;
			from :=
				begin match trace.Out.events with
				| [] -> !from
				| (id, _) :: ms -> Int64.add 1L (List.fold_left max id (List.map fst ms))
				end;
			return ()
	done

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Trace messages";

	Lwt_unix.run (main ()) 
