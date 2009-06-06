(** Wrapper around gpg *)

(* Copyright (C) XenSource 2007 *)

open Stringext
open Pervasiveext

module D = Debug.Debugger(struct let name="gpg" end)
open D

(* Set from config file: *)
let filename = ref ""

let gpg_binary_path = "/usr/bin/gpg"
let gpg_limiter_path = "/opt/xensource/libexec/gpg-limiter.sh"
let gpg_homedir = "/opt/xensource/gpg/"
let gpg_pub_keyring = gpg_homedir^"pubring.gpg"
let allowed_gpg_checksum =
	[ "be00ee82bffad791edfba477508d5d84"; (* product version *)
	  "f52886b87126c06d419f408e32268b4e"; (* 64 bit product version *)
	  "aa27ac0b0ebfd1278bf2386c343053db"; (* debian developer version *)
	  "044d1327ea42400ac590195e0ec1e7e6"; ]

let allowed_limiter_checksum = ["2cc874e5610243f86470639104f56c92"]

exception InvalidSignature

let parse_gpg_status status_data =
  let lines = String.split '\n' status_data in
  let status_contains substr =
    List.exists (fun s -> String.startswith substr s) lines in
  if not (status_contains "[GNUPG:] GOODSIG" && status_contains "[GNUPG:] TRUST_ULTIMATE") then
    raise InvalidSignature;
  let validsig = "[GNUPG:] VALIDSIG" in
  if status_contains validsig then
    let validsigline = List.find (fun s -> String.startswith validsig s) lines in
    match String.split ' ' validsigline with
	_::_::fingerprint::_ -> Some fingerprint
      | _ -> None
  else
    None	

let simple_checksum file = Digest.to_hex (Digest.file file)

let common ty filename signature size f =
  let tmp_file, tmp_oc = Filename.open_temp_file ~mode:[Open_binary] "gpg" "" in
  let result_in = Unix.descr_of_out_channel tmp_oc in
  let result_out = Unix.openfile tmp_file [Unix.O_RDONLY] 0o0 in 
  Unix.unlink tmp_file; 
  (* no need to close the 'tmp_oc' -> closing the fd is enough *)
  let status_out, status_in = Unix.pipe() in
  (* from the parent's PoV *)
  let fds_to_close = ref [ result_out; result_in; status_out; status_in ] in
  let close' fd = 
    if List.mem fd !fds_to_close 
    then (Unix.close fd; fds_to_close := List.filter (fun x -> x <> fd) !fds_to_close) in

  let gpg_args = match ty with
    | `signed_cleartext ->
        [
		  "--homedir"; gpg_homedir;
		  "--no-default-keyring";
		  "--keyring"; gpg_pub_keyring;
		  "--status-fd"; string_of_int (Unixext.int_of_file_descr status_in);
		  "--decrypt"; filename
		]
    | `detached_signature ->
        [
          filename; Int64.to_string size;
		  "--homedir"; gpg_homedir;
		  "--no-default-keyring";
		  "--keyring"; gpg_pub_keyring;
		  "--status-fd"; string_of_int (Unixext.int_of_file_descr status_in);
		  "--verify"; signature
		]
  in
  (* Let's check the checksums of gpg and its helper script for oem *)
  let gpg_binary_sum = simple_checksum gpg_binary_path in
  let gpg_limiter_sum = simple_checksum gpg_limiter_path in
  if not (List.mem gpg_binary_sum allowed_gpg_checksum) then
    raise InvalidSignature;
  if not (List.mem gpg_limiter_sum allowed_limiter_checksum) then
    raise InvalidSignature;

  let gpg_path = 
    match ty with
      | `signed_cleartext -> gpg_binary_path
      | `detached_signature -> gpg_limiter_path
  in

  finally  (* make sure I close all my open fds in the end *)
    (fun () ->
       Forkhelpers.with_dev_null (* open /dev/null *)
	 (fun dev_null ->
	    (* Capture stderr output for logging *)
	    match Forkhelpers.with_logfile_fd "gpg"
	      (fun log_fd ->
		 let pid = Forkhelpers.safe_close_and_exec
		   [ Forkhelpers.Dup2(result_in, Unix.stdout);
		     Forkhelpers.Dup2(log_fd, Unix.stderr);
		     Forkhelpers.Close(result_out);
		     Forkhelpers.Close(status_out) ]
		   [ Unix.stdout; Unix.stderr; status_in ] (* close all but these *)
		   gpg_path gpg_args in
		 (* parent *)
		 List.iter close' [ result_in; status_in ];
		 finally (* always waitpid eventually *)
		   (fun () ->
		      let gpg_status = Unixext.read_whole_file 500 500 status_out in
		      let fingerprint = parse_gpg_status gpg_status in
		      f fingerprint result_out)
		   (fun () -> Forkhelpers.waitpid pid)) with
	      | Forkhelpers.Success(_, x) -> debug "gpg subprocess succeeded"; x
	      | Forkhelpers.Failure(log, Forkhelpers.Subprocess_failed 2) ->
		  (* Happens when gpg cannot find a readable signature *)
		  raise InvalidSignature
	      | Forkhelpers.Failure(log, exn) ->
		  debug "Error from gpg: %s" log;
		  raise exn))
    (fun () -> List.iter Unix.close !fds_to_close)

let with_signed_cleartext filename f =
  common `signed_cleartext filename "" Int64.zero f

let with_detached_signature filename signature size f =
  common `detached_signature filename signature size f
