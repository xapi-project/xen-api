(*
 * Copyright (c) 2012 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)


module Unbuffered_IO = struct
	(** Use as few Unix.{read,write} calls as we can (for efficiency) without
		explicitly buffering the stream beyond the HTTP headers. This will
		allow us to consume the headers and then pass the file descriptor
		safely to another process *)

	type 'a t = 'a

	let (>>=) x f = f x

	let return x = x

	let iter = List.iter

	type ic = {
		mutable header_buffer: string option; (** buffered headers *)
		mutable header_buffer_idx: int;       (** next char within the buffered headers *)
		fd: Unix.file_descr;                  (** the underlying file descriptor *)
	}

	type oc = Unix.file_descr

	let read_http_headers fd =
		let buf = Buffer.create 128 in
		(* We can safely read everything up to this marker: *)
		let end_of_headers = "\r\n\r\n" in
		let tmp = String.make (String.length end_of_headers) '\000' in
		let module Scanner = struct
			type t = {
				marker: string;
				mutable i: int;
			}
			let make x = { marker = x; i = 0 }
			let input x c =
				if c = x.marker.[x.i] then x.i <- x.i + 1 else x.i <- 0
			let remaining x = String.length x.marker - x.i
			let matched x = x.i = String.length x.marker
			let to_string x = Printf.sprintf "%d" x.i
		end in
		let marker = Scanner.make end_of_headers in

		while not(Scanner.matched marker) do
			(* We may be part way through reading the end of header marker, so
			   be pessimistic and only read enough bytes to read until the end of
			   the marker. *)
			let safe_to_read = Scanner.remaining marker in

			let n = Unix.read fd tmp 0 safe_to_read in
			if n = 0 then raise End_of_file;

			for j = 0 to n - 1 do
				Scanner.input marker tmp.[j];
				Buffer.add_char buf tmp.[j]
			done;
		done;
		Buffer.contents buf

	let crlf = Re_str.regexp_string "\r\n"

	(* We assume read_line is only used to read the HTTP header *)
	let rec read_line ic = match ic.header_buffer, ic.header_buffer_idx with
		| None, _ ->
			ic.header_buffer <- Some (read_http_headers ic.fd);
			read_line ic
		| Some buf, i when i < (String.length buf) ->
			begin
				try
					let eol = Re_str.search_forward crlf buf i in
					let line = String.sub buf i (eol - i) in
					ic.header_buffer_idx <- i + 4;
					Some line
				with Not_found -> Some ""
			end
		| Some _, _ ->
			Some ""

	let rec read_exactly ic buf ofs len =
		let n = Unix.read ic.fd buf ofs len in
		let remaining = len - n in
		if remaining > 0
		then read_exactly ic buf (ofs + n) (len - n)

	let read ic n =
		let buf = String.make n '\000' in
		let actually_read = Unix.read ic.fd buf 0 n in
		if actually_read = n
		then buf
		else String.sub buf 0 actually_read

	let write oc x = Unix.write oc x 0 (String.length x)

	let open_uri uri f =
		let handle_socket s =
			try
				let result = f s in
				Unix.close s;
				result
			with e ->
				Unix.close s;
				raise e in
		match Uri.scheme uri with
			| Some "http" ->
				begin match Uri.host uri, Uri.port uri with
					| Some host, Some port ->
						let inet_addr = Unix.inet_addr_of_string host in
						let sockaddr = Unix.ADDR_INET(inet_addr, port) in
						let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
						Unix.connect s sockaddr;
						handle_socket s
					| _, _ -> failwith (Printf.sprintf "Failed to parse host and port from URI: %s" (Uri.to_string uri))
				end
			| Some "file" ->
				let filename = Uri.path_and_query uri in
				let sockaddr = Unix.ADDR_UNIX filename in
				let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
				Unix.connect s sockaddr;
				handle_socket s
			| Some x -> failwith (Printf.sprintf "Unsupported URI scheme: %s" x)
			| None -> failwith (Printf.sprintf "Failed to parse URI: %s" (Uri.to_string uri))

end

module Buffered_IO = struct
	type 'a t = 'a

	let (>>=) x f = f x

	let return x = x

	let iter = List.iter

	type ic = in_channel
	type oc = out_channel

	let read_line ic = try Some(input_line ic) with End_of_file -> None

	let read_exactly ic buf ofs len = try really_input ic buf ofs len; true with _ -> false

	let read ic n =
		let buf = String.make n '\000' in
		let actually_read = input ic buf 0 n in
		if actually_read = n
		then buf
		else String.sub buf 0 actually_read

	let write oc x = output_string oc x; flush oc

	let open_uri uri f = Unbuffered_IO.open_uri uri (fun fd -> f (Unix.in_channel_of_descr fd) (Unix.out_channel_of_descr fd))
end
