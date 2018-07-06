(*
 *	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
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
 * SHA1sum - test ocaml SHA1 binding
 *)

open Printf

let printfct get_digest file =
	let digest = get_digest file in
	printf "%s  %s\n" digest file

let checkfct get_digest file =
	let chan = open_in file in

	let nb = ref 0 and failed = ref 0 in
	begin try while true
	do
		let line = input_line chan in
		Scanf.sscanf line "%s %s" (fun hex file ->
			let digest = get_digest file in
			let fail = digest <> hex in
			if fail then
				incr failed;
			incr nb;
			printf "%s: %s\n" file
			       (if fail then "FAILED" else "OK")
		)
	done
	with End_of_file -> () end;
	if !failed > 0 then
		eprintf "sha1sum: WARNING: %d of %d computed checksums did NOT match\n"
		        !failed !nb;
	close_in chan

(* main fct *)
let _ =
	let files = ref [] in
	let eoa = ref false and check = ref false in

	(* parse arg *)
	for i = 1 to Array.length Sys.argv - 1
	do
		let opt = Sys.argv.(i) in
		if !eoa then
			files := opt :: !files
		else
			match opt with
			| "--check" | "-c"  -> check := true
			| "--binary" | "-b" -> ()
			| "--text" | "-t"   -> ()
			| "--"              -> eoa := true
			| ""                -> ()
			| s                 ->
				if s.[0] = '-' then
					eprintf "unknown option: %s" s
				else
					files := opt :: !files
	done;

	let md5 file = Digest.to_hex (Digest.file file) in
	let sha1 file = Sha1.to_hex (Sha1.file file) in
	let sha256 file = Sha256.to_hex (Sha256.file file) in
	let sha512 file = Sha512.to_hex (Sha512.file file) in

	let basename = Filename.basename Sys.argv.(0) in
	let prog =
		if Sys.os_type = "Win32" then
			try Filename.chop_extension basename
			with Invalid_argument _ -> basename
		else
			basename
		in
	let sha = match prog with
		| "sha512sum" -> sha512
		| "sha256sum" -> sha256
		| "sha1sum"   -> sha1
		| "md5sum"    -> md5
		| _           -> sha1 in

	let execfct_with_catch file =
		try
			(if !check then checkfct else printfct) sha file
		with
			exn -> eprintf "error: %s: %s\n" file
			               (Printexc.to_string exn) in

	(* apply function on every file *)
	List.iter (fun file -> execfct_with_catch file) (List.rev !files)
