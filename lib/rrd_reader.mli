module type TRANSPORT = sig
	type id_t

	type state_t

	val init: id_t -> state_t

	val cleanup: id_t -> state_t -> unit

	val expose: state_t -> Cstruct.t
end

module File : sig
	type id_t = string
	type state_t = Unix.file_descr

	val init: id_t -> state_t

	val cleanup: id_t -> state_t -> unit

	val expose: state_t -> Cstruct.t
end

module Page : sig
	type id_t = int * (int list)
	type state_t = Gnt.Gnttab.Local_mapping.t

	val init: id_t -> state_t

	val cleanup: id_t -> state_t -> unit

	val expose: state_t -> Cstruct.t
end

type reader = {
	read_payload: unit -> Rrd_protocol.payload;
	cleanup: unit -> unit;
}

module Make (T: TRANSPORT) : sig
	val create: T.id_t -> Rrd_protocol.protocol -> reader
end

module FileReader : sig
	val create: File.id_t -> Rrd_protocol.protocol -> reader
end

module PageReader : sig
	val create: Page.id_t -> Rrd_protocol.protocol -> reader
end
