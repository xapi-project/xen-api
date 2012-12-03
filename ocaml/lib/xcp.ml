module Result = struct
	type ('a, 'b) t =
		| Ok of 'a
		| Error of 'b

	let bind t f = match t with
		| Error e -> Error e
		| Ok x -> f x

	let (>>=) = bind

	let return x = Ok x

	let fail e = Error e

end

type ('a, 'b) t = ('a, 'b) Result.t

module type M = sig

  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

end

module type RPC = sig
	include M

	val rpc : Rpc.call -> Rpc.response t
end

exception Unknown_method of string

exception Internal_error of string

exception Channel_setup_failed

let ( |> ) a b = b a

module Channel = struct
	type t = Unix.file_descr

	type protocol =
		| TCP_proxy of string * int             (** IP, port *)
		| V4V_proxy of int * int                (** domid, port *)
		| Unix_sendmsg of int * string * string (** domid, path, token *)
	with rpc


	let _proxy = "/usr/local/bin/proxy"

	let exec cmd args =
		match Unix.fork() with
			| 0 ->
				Unix.execv cmd (Array.of_list (cmd :: args))
			| pid -> pid

	let int_of_file_descr (fd: Unix.file_descr) : int = Obj.magic fd

	let export fd =
		let ip = "127.0.0.1" in
		let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		Unix.bind s (Unix.ADDR_INET(Unix.inet_addr_of_string ip, 0));
		Unix.listen s 5;
		let port = match Unix.getsockname s with
			| Unix.ADDR_INET(_, port) -> port
			| _ -> assert false in
		let args = [
			"-accept"; string_of_int (int_of_file_descr s);
			"-proxy"; string_of_int (int_of_file_descr fd);
		] in
		let _ = exec _proxy args in
		(* fork to a single one-shot proxy process, handing over fd and s *)
		Unix.close s;
		[
			TCP_proxy(ip, port);
		]

	type protocols = protocol list with rpc

	let rpc_of_t fd =
		(* Advertise the fd's availability over a list of protocols *)
		rpc_of_protocols (export fd)

	let t_of_rpc x =
		let protocols = protocols_of_rpc x in
		(* Choose the best transport mechanism from the list of options *)
		try
			match List.find (function TCP_proxy(_, _)-> true | _ -> false) protocols with
				| TCP_proxy(ip, port) ->
					let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
					Unix.connect s (Unix.ADDR_INET(Unix.inet_addr_of_string ip, port));
					s
				| _ -> assert false
		with Not_found ->
			raise Channel_setup_failed

	let example = Unix.stdout
end
