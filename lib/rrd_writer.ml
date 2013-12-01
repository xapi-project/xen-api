module type TRANSPORT = sig
	(** An identifier needed to open the resource. *)
	type id_t
	(** A handle to an open resource. *)
	type state_t

	(** Open a resource for writing, given its identifier. *)
	val init: id_t -> state_t
	(** Cleanup an open resource when it is no longer needed. *)
	val cleanup: state_t -> unit

	(** Get a function which, when given an integer representing a number of
	 *  bytes to be written, will return a Cstruct of that size (or potentially
	 *  throw an exception if the transport method determines that that size is
	 *  too large. *)
	val get_allocator: state_t -> (int -> Cstruct.t)
end

module File = struct
	(** Filesystem path. *)
	type id_t = string
	(** Path to shared file (needed to unlink) * fd for writing to the
	 *  shared file. *)
	type state_t = string * Unix.file_descr

	let init path = path, Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o600

	let cleanup (path, fd) =
		Unix.close fd;
		Unix.unlink path

	(** This assumes there's no limit to the size of file which can be used. *)
	let get_allocator (_, fd) =
		if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
			failwith "lseek";
		let alloc_cstruct size =
			let mapping = Bigarray.(Array1.map_file fd char c_layout true size) in
			Cstruct.of_bigarray mapping
		in
		alloc_cstruct
end

module Page = struct
	open Gnt

	(** remote domid * page count *)
	type id_t = (int * int)
	type state_t = Gntshr.share

	let init (domid, count) =
		let share =
			Gnt_helpers.with_gntshr
				(fun gntshr -> Gntshr.share_pages_exn gntshr domid count false)
		in
		Printf.printf
			"sharing pages with references [%s] with domid %d\n%!"
			(String.concat ";"
				(List.map string_of_int share.Gntshr.refs))
			domid;
		share

	let cleanup share =
		Gnt_helpers.with_gntshr
			(fun gntshr -> Gntshr.munmap_exn gntshr share)

	(** The allocator returns a Cstruct mapping all of the shared memory, unless
	 *  the size requested is greater than the size of this memory in which case
	 *  the allocator fails. *)
	let get_allocator share =
		let alloc_cstruct size =
			if size > Bigarray.Array1.dim share.Gntshr.mapping then
				failwith "not enough memory";
			Cstruct.of_bigarray share.Gntshr.mapping
		in
		alloc_cstruct
end

module Make (P: Rrd_protocol.PROTOCOL) (T: TRANSPORT) = struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> T.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup (); exit 0))

	let start interval id generate_payload =
		setup_signals ();
		let state = T.init id in
		cached_state := Some state;
		try
			while true do
				let allocator = T.get_allocator state in
				P.write_payload allocator (generate_payload ());
				Thread.delay interval
			done
		with e ->
			T.cleanup state;
			raise e
end
