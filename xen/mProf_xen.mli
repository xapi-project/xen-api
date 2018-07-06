(** Trace processes on Xen, keeping the results in a memory region
    that is shared with another domain. *)

type log_buffer =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external get_monotonic_time : unit -> int64 = "caml_get_monotonic_time"
val timestamper : EndianBigstring.bigstring -> int -> unit
val make_shared_buffer : size:int -> Io_page.t
type gntref = int
module type GNTSHR =
  sig
    val get : unit -> gntref Lwt.t
    val grant_access :
      domid:int -> writable:bool -> gntref -> Io_page.t -> unit
  end
module type XS =
  sig
    type client
    type handle
    val make : unit -> client Lwt.t
    val read : handle -> string -> string Lwt.t
    val write : handle -> string -> string -> unit Lwt.t
    val immediate : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
    val transaction : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
    val getdomainpath : handle -> int -> string Lwt.t
  end
val share_with :
  (module GNTSHR) -> (module XS) -> domid:int -> Io_page.t -> unit Lwt.t
