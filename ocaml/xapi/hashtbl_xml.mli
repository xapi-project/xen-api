(** Thrown when parsing malformed XML *)
exception Unmarshall_error of string

(** Type of hashtables supported by this module *)
type h = (string, string) Hashtbl.t

(** Serialise the given hashtbl to the given Xmlm output *)
val to_xml: h -> Xmlm.output -> unit

(** Deserialise a hashtbl from the given Xmlm input *)
val of_xml: Xmlm.input -> h
