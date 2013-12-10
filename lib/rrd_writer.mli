module type TRANSPORT = sig
	type id_t

	type state_t

	val init: id_t -> state_t

	val cleanup: id_t -> state_t -> unit

	val get_allocator: state_t -> (int -> Cstruct.t)
end

module File : sig
	type id_t = string
	type state_t = Unix.file_descr

	val init: id_t -> state_t

	val cleanup: id_t -> state_t -> unit

	val get_allocator: state_t -> (int -> Cstruct.t)
end

module Page : sig
	type id_t = int * int
	type state_t = Gnt.Gntshr.share

	val init: id_t -> state_t

	val cleanup: id_t -> state_t -> unit

	val get_allocator: state_t -> (int -> Cstruct.t)
end

type writer = {
	write_payload: Rrd_protocol.payload -> unit;
	cleanup: unit -> unit;
}

module Make (T: TRANSPORT) : sig
	val create: T.id_t -> Rrd_protocol.protocol -> writer
end

module FileWriter : sig
	val create: File.id_t -> Rrd_protocol.protocol -> writer
end

module PageWriter : sig
	val create: Page.id_t -> Rrd_protocol.protocol -> writer
end
