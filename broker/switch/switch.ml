open Lwt
open Cohttp

let debug fmt = Logging.debug "server_xen" fmt
let warn  fmt = Logging.warn  "server_xen" fmt
let error fmt = Logging.error "server_xen" fmt

let message_logger = Logging.create 512
let message conn_id (fmt: (_,_,_,_) format4) =
    Printf.ksprintf message_logger.Logging.push ("[%3d] " ^^ fmt) conn_id

let syslog = Lwt_log.syslog ~facility:`Local3 ()

let rec logging_thread logger =
    lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
            (fun x ->
                lwt () = Lwt_log.log ~logger:!Lwt_log.default ~level:Lwt_log.Notice x in
				return ()
			) lines in
	logging_thread logger


let make_path p = Store.Path.create p (Store.Path.getdomainpath 0)


let startswith prefix x = String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)


let port = ref 8080

open Cohttp_lwt_unix

let no_name () =
	Server.respond_string ~status:`Bad_request ~body:"no name has been bound" ()

let no_binding x =
	Server.respond_string ~status:`Not_found ~body:(Printf.sprintf "name %s is not bound" x) ()

let redirect x =
	let headers = Header.add (Header.init()) "Location" x in
	Server.respond_string ~headers ~status:`Found ~body:"" ()

let make_unique_id =
	let counter = ref 0L in
	fun () ->
		let result = !counter in
		counter := Int64.add 1L !counter;
		result

let make_fresh_name () = Printf.sprintf "client-%Ld" (make_unique_id ())

module Int64Map = Map.Make(struct type t = int64 let compare = Int64.compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = String.compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)

module StringRelation = struct
	(** Store a (a: string, b: string) relation R such that given
		an a, finding the largest set bs such that
          \forall b \in bs. (a, b)\in R
		and v.v. on b are efficient. *)

	type t = {
		a_to_b: StringSet.t StringMap.t;
		b_to_a: StringSet.t StringMap.t;
	}
	let empty = {
		a_to_b = StringMap.empty;
		b_to_a = StringMap.empty;
	}
	let to_string t =
		let set xs = String.concat "; " (StringSet.fold (fun x acc -> x :: acc) xs []) in
		Printf.sprintf "{ %s }" 
			(String.concat "; "
				 (StringMap.fold (fun k v acc -> 
					 Printf.sprintf "%s: { %s }" k (set v) :: acc
				  ) t.a_to_b []
				 )
			)

	let mapping_of x map =
		if StringMap.mem x map
		then StringMap.find x map
		else StringSet.empty
	let get_bs a t = mapping_of a t.a_to_b
	let get_as b t = mapping_of b t.b_to_a
	let add a b t =
		{
			a_to_b = StringMap.add a (StringSet.add b (get_bs a t)) t.a_to_b;
			b_to_a = StringMap.add b (StringSet.add a (get_as b t)) t.b_to_a;
		}
	let of_list = List.fold_left (fun t (a, b) -> add a b t) empty
	let to_list t = StringMap.fold (fun a bs acc -> StringSet.fold (fun b acc -> (a, b) :: acc) bs acc) t.a_to_b []

	let remove_a a t =
		let bs = get_bs a t in
		{
			a_to_b = StringMap.remove a t.a_to_b;
			b_to_a =
				StringSet.fold
					(fun b acc ->
						let as' = StringSet.remove a (mapping_of b acc) in
						if as' = StringSet.empty
						then StringMap.remove b acc
						else StringMap.add b as' acc
					) bs t.b_to_a;
		}
	let flip t = {
		a_to_b = t.b_to_a;
		b_to_a = t.a_to_b;
	}
	let remove_b b t = flip (remove_a b (flip t))

	let equal t1 t2 =
		true
		&& StringMap.equal StringSet.equal t1.a_to_b t2.a_to_b
		&& StringMap.equal StringSet.equal t1.b_to_a t2.b_to_a
	let always_true t = equal t (flip (of_list (List.map (fun (a, b) -> b, a) (to_list t))))

	let test_inputs = List.map of_list [
		[];
		[ "foo", "bar" ];
		[ "foo", "bar"; "foo", "anotherbar" ];
		[ "foo", "bar"; "anotherfoo", "bar" ];
		[ "foo", "bar"; "bar", "foo" ]
	]
end

type origin =
	| Anonymous of int (** An un-named connection, probably a temporary client connection *)
	| Name of string   (** A service with a well-known name *)
with rpc

module Entry = struct
	type t = {
		origin: origin;
		time: float; (* XXX this is for judging age: use oclock/clock_monotonic *)
		message: Protocol.Message.t;
	} with rpc

	let make origin message =
		let time = 0. in
		{ origin; time; message }
end

module Subscription = struct
	(* Session token -> queue bindings *)
	let t = ref (StringRelation.empty)

	let session_to_wakeup : (string, unit Lwt.u) Hashtbl.t = Hashtbl.create 128

	let add session subscription =
		t := StringRelation.add session subscription !t;
		if Hashtbl.mem session_to_wakeup session then begin
			Lwt.wakeup_later (Hashtbl.find session_to_wakeup session) ()
		end

	let get session = StringRelation.get_bs session !t

	let remove subscription =
		t := StringRelation.remove_b subscription !t

end

module Connections = struct
	(* Connection id -> session *)
	let connections : string IntMap.t ref = ref IntMap.empty

	(* session -> active connection ids *)
	let sessions : (string, IntSet.t) Hashtbl.t = Hashtbl.create 128

	let get_origin conn_id =
		if IntMap.mem conn_id !connections
		then Name (IntMap.find conn_id !connections)
		else Anonymous conn_id

	let get_session conn_id =
		IntMap.find conn_id !connections

	let add conn_id session =
		debug "Connections.add %d -> %s" conn_id session;
		connections := IntMap.add conn_id session !connections;
		let existing =
			if Hashtbl.mem sessions session
			then Hashtbl.find sessions session
			else IntSet.empty in
		Hashtbl.replace sessions session (IntSet.add conn_id existing)

	let remove conn_id =
		let session = get_session conn_id in
		let existing =
			if Hashtbl.mem sessions session
			then Hashtbl.find sessions session
			else IntSet.empty in
		connections := IntMap.remove conn_id !connections;
		let remaining = IntSet.remove conn_id existing in
		debug "Connections.remove %d (session %s size %d)" conn_id session (IntSet.cardinal remaining);
		if remaining = IntSet.empty
		then Hashtbl.remove sessions session
		else Hashtbl.replace sessions session remaining

	let is_session_active session = Hashtbl.mem sessions session

end

module Q = struct

	let queues : (string, Entry.t Int64Map.t) Hashtbl.t = Hashtbl.create 128
	let message_id_to_queue : string Int64Map.t ref = ref Int64Map.empty

	type wait = {
		c: unit Lwt_condition.t;
		m: Lwt_mutex.t
	}

	let waiters = Hashtbl.create 128

	let exists name = Hashtbl.mem queues name

	let add name =
		if not(exists name) then begin
			Hashtbl.replace queues name Int64Map.empty;
			Hashtbl.replace waiters name {
				c = Lwt_condition.create ();
				m = Lwt_mutex.create ()
			}
		end

	let get name =
		if exists name
		then Hashtbl.find queues name
		else Int64Map.empty


	let remove name =
		let entries = get name in
		Int64Map.iter (fun id _ ->
			message_id_to_queue := Int64Map.remove id !message_id_to_queue
		) entries;
		Hashtbl.remove queues name;
		Hashtbl.remove waiters name;
		Subscription.remove name

	let ack id =
		let name = Int64Map.find id !message_id_to_queue in
		if exists name then begin
			let q = get name in
			message_id_to_queue := Int64Map.remove id !message_id_to_queue;
			Printf.fprintf stderr "Removing id %Ld from queue %s\n%!" id name;
			Hashtbl.replace queues name (Int64Map.remove id q);
		end

	let wait from name =
		if Hashtbl.mem waiters name then begin
			let w = Hashtbl.find waiters name in
			Lwt_mutex.with_lock w.m
				(fun () ->
					let rec loop () =
						let _, _, not_seen = Int64Map.split from (get name) in
						if not_seen = Int64Map.empty then begin
							lwt () = Lwt_condition.wait ~mutex:w.m w.c in
							loop ()
						end else return () in
					loop ()
				)
		end else begin
			let t, _ = Lwt.task () in
			t (* block forever *)
		end

	let send conn_id name data =
		(* If a queue doesn't exist then drop the message *)
		if exists name then begin
			let w = Hashtbl.find waiters name in
			Lwt_mutex.with_lock w.m
				(fun () ->
					let q = get name in
					let origin = Connections.get_origin conn_id in
					let id = make_unique_id () in
					message_id_to_queue := Int64Map.add id name !message_id_to_queue;
					Hashtbl.replace queues name (Int64Map.add id (Entry.make origin data) q);
					Lwt_condition.broadcast w.c ();
					return ()
				)
		end else return ()

end

module Private_queue = struct

	(* Session -> set of queues which will be GCed on session cleanup *)
	let private_queues : (string, StringSet.t) Hashtbl.t = Hashtbl.create 128

	let add session name =
		let existing =
			if Hashtbl.mem private_queues session
			then Hashtbl.find private_queues session
			else StringSet.empty in
		Hashtbl.replace private_queues session (StringSet.add name existing)

	let remove session =
		if Hashtbl.mem private_queues session then begin
			let qs = Hashtbl.find private_queues session in
			StringSet.iter
				(fun name ->
					debug "Deleting private queue: %s" name;
					Q.remove name;
				) qs;
			Hashtbl.remove private_queues session
		end
end

module Diagnostics = struct
	type queue = (int64 * Entry.t) list with rpc
	let queue q = Int64Map.fold (fun i e acc -> (i, e) :: acc) q []

	type queues = (string * queue) list with rpc

	let snapshot () = Hashtbl.fold (fun n q acc -> (n, queue q) :: acc) Q.queues []
end



let make_server () =
	debug "Started server on unix domain socket";
    let (_: 'a) = logging_thread Logging.logger in
    let (_: 'a) = logging_thread message_logger in

  	(* (Response.t * Body.t) Lwt.t *)
	let callback conn_id ?body req =
		let open Protocol in
		lwt body = match body with
			| None -> return None
			| Some b ->
				lwt s = Body.string_of_body (Some b) in
				return (Some s) in
		match In.of_request (req, body) with
		| None ->
			Server.respond_not_found ~uri:(Request.uri req) ()
		| Some request ->
			message conn_id "%s" (Jsonrpc.to_string (In.rpc_of_t request));
			begin match request with
			| In.Login session ->
				(* associate conn_id with 'session' *)
				Connections.add conn_id session;
				Out.to_response Out.Login
			| In.Create name ->
				let name = begin match name with
				| Some name ->
					name
				| None ->
					(* Create a session-local private queue name *)
					let name = make_fresh_name () in
					let session = Connections.get_session conn_id in
					Private_queue.add session name;
					name
				end in
				Q.add name;
				Out.to_response (Out.Create name)
			| In.Subscribe name ->
				let session = Connections.get_session conn_id in
				Subscription.add session name;
				Out.to_response Out.Subscribe
			| In.Ack id ->
				Q.ack id;
				Out.to_response Out.Ack
			| In.Transfer(from, timeout) ->
				let session = Connections.get_session conn_id in

				let start = Unix.gettimeofday () in
				let rec wait () =
					let names = Subscription.get session in
					let not_seen = StringSet.fold (fun name map ->
						let q = Q.get name in
						let _, _, not_seen = Int64Map.split from q in
						Int64Map.fold Int64Map.add map not_seen
					) names Int64Map.empty in
					if not_seen <> Int64Map.empty
					then return not_seen
					else
						let remaining_timeout = max 0. (start +. timeout -. (Unix.gettimeofday ())) in
						if remaining_timeout <= 0.
						then return Int64Map.empty
						else
							let timeout = Lwt.map (fun () -> `Timeout) (Lwt_unix.sleep remaining_timeout) in
							let more = StringSet.fold (fun name acc ->
								Lwt.map (fun () -> `Data) (Q.wait from name) :: acc
							) names [] in
							let t, u = Lwt.task () in
							Hashtbl.replace Subscription.session_to_wakeup session u;
							let sub = Lwt.map (fun () -> `Subscription) t in
							try_lwt
								match_lwt Lwt.pick (sub :: timeout :: more) with
								| `Timeout -> return Int64Map.empty
								| `Data ->
									wait ()
								| `Subscription ->
									wait ()
							finally
								Hashtbl.remove Subscription.session_to_wakeup session;
								return ()
							in
				lwt messages = wait () in
				let transfer = {
					Out.messages = Int64Map.fold (fun id e acc -> (id, e.Entry.message) :: acc) messages [];
				} in
				Out.to_response (Out.Transfer transfer)

			| In.Send (name, data) ->
				lwt () = Q.send conn_id name data in
				Out.to_response Out.Send
			| In.Diagnostics ->
				let d = Diagnostics.(Jsonrpc.to_string (rpc_of_queues (snapshot ()))) in
				Out.to_response (Out.Diagnostics d)
			end
			in
			let conn_closed conn_id () =
				let session = Connections.get_session conn_id in
				Connections.remove conn_id;
				if not(Connections.is_session_active session) then begin
					debug "Session %s cleaning up" session;
					Private_queue.remove session
				end in

			debug "Message switch starting";
			let (_: 'a Lwt.t) = logging_thread Logging.logger in

			let config = { Server.callback; conn_closed } in
			server ~address:"127.0.0.1" ~port:!port config
    
let _ =
	Arg.parse [
		"-port", Arg.Set_int port, "port to listen on";
	] (fun x -> Printf.fprintf stderr "Ignoring: %s" x)
		"A simple message switch";

	Lwt_unix.run (make_server ()) 

