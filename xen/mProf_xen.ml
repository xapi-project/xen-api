(* Copyright (C) 2014, Thomas Leonard *)

open Bigarray
type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

external get_monotonic_time : unit -> int64 = "caml_get_monotonic_time"

let timestamper buf off =
  EndianBigstring.LittleEndian.set_int64 buf off (get_monotonic_time ())

let make_shared_buffer ~size =
  let open Io_page in
  let n_pages = round_to_page_size size / round_to_page_size 1 in
  get n_pages

type gntref = int

(* ocaml-gnt and mirage-xen both depend on mirage-platform, so pass them in here
 * to avoid a cycle (alternatively, we could move mirage-profile.xen to a separate
 * OPAM package). *)
module type GNTSHR = sig
  val get : unit -> gntref Lwt.t
  val grant_access : domid:int -> writable:bool -> gntref -> Io_page.t -> unit
end

module type XS = sig
  type client
  type handle

  val make : unit -> client Lwt.t
  val read : handle -> string -> string Lwt.t
  val write : handle -> string -> string -> unit Lwt.t
  val immediate : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
  val transaction : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
  val getdomainpath : handle -> int -> string Lwt.t
end

let share_with (module Gntshr : GNTSHR) (module Xs : XS) ~domid buffer =
  let pages = Io_page.to_pages buffer in
  let open Lwt in

  let refs =
    pages |> Lwt_list.map_s (fun page ->
      Gntshr.get () >>= fun gnt ->
      Gntshr.grant_access ~domid ~writable:false gnt page;
      return gnt
    ) in
  refs >>= fun refs ->

  let ring_ref = refs
    |> List.map string_of_int
    |> String.concat "," in

  Xs.make ()
  >>= fun c ->
  Xs.(immediate c (fun h -> read h "domid")) >>= fun my_domid ->
  Xs.(immediate c (fun h -> getdomainpath h (int_of_string my_domid))) >>= fun domainpath ->
  let xs_path = Printf.sprintf "%s/data/mprof" domainpath in
  Xs.(transaction c (fun h -> write h (xs_path ^ "/ring-ref") ring_ref)) >>= fun () ->
  return ()
