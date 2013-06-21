open Xentoollog
open Xenops_utils

module D = Debug.Make(struct let name = "libxenlight" end)
open D

let vmessage min_level level errno ctx msg =
	let ctx_str = match ctx with None -> [] | Some s -> [Printf.sprintf "%s" s] in
	let errno_str = match errno with None -> [] | Some s -> [Printf.sprintf "errno=%d" s] in
	let prefix = String.concat ": " (ctx_str @ errno_str) in
	if compare min_level level <= 0 then begin
		match level with
		| Debug ->
			debug "%s: %s\n" prefix msg;
		| Verbose | Detail | Progress | Info | Notice ->
			info "%s: %s\n" prefix msg;
		| Warn ->
			warn "%s: %s\n" prefix msg;
		| Error | Critical ->
			error "%s: %s\n" prefix msg;
	end

let progress ctx what percent dne total =
	let nl = if dne = total then "\n" else "" in
	debug "progress: %s %d%% (%Ld/%Ld)%s" what percent dne total nl

let create ?(level=Info) () =
	let cbs = {
		vmessage = vmessage level;
		progress = progress;
	} in
	create "Xentoollog.xenopsd_logger" cbs

