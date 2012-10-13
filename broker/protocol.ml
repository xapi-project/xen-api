open Cohttp
open Cohttp_lwt_unix
open Lwt

module Frame = struct
	type t =
	| Send of string * string
	| Transfer of string (* ack to *)
	| Connect
	| Bind of string

	let rec split ?limit:(limit=(-1)) c s =
		let i = try String.index s c with Not_found -> -1 in
		let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
		if i = -1 || nlimit = 0 then
			[ s ]
		else
			let a = String.sub s 0 i
			and b = String.sub s (i + 1) (String.length s - i - 1) in
			a :: (split ~limit: nlimit c b)

	let of_request req = match Request.meth req, split '/' (Request.path req) with
		| `GET, [ ""; "bind"; name ] -> Some (Bind name)
		| `GET, [ ""; "connection" ] -> Some Connect
		| `GET, [ ""; "transfer"; ack_to ] -> Some (Transfer ack_to)
		| `GET, [ ""; "send"; name; data ] -> Some (Send (name, data))
		| _, _ -> None

	let to_request = function
		| Bind name ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/bind/%s" name) ())
		| Connect ->
			Request.make ~meth:`GET (Uri.make ~path:"/connection" ())
		| Transfer ack_to ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/transfer/%s" ack_to) ())
		| Send (name, data) ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/send/%s/%s" name data) ())
end




module Connection = struct
	type t = Lwt_unix.file_descr

	let make port =
		let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port) in
		let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
		lwt () = Lwt_unix.connect fd sockaddr in
		return fd

end
