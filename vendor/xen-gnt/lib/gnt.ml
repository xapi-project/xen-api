(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2012-2014 Citrix Inc
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
 *)

open Lwt

type gntref = int
type domid = int

let console = 0 (* public/grant_table.h:GNTTAB_RESERVED_CONSOLE *)
let xenstore = 1 (* public/grant_table.h:GNTTAB_RESERVED_XENSTORE *)

type grant_handle (* handle to a mapped grant *)

module Gnttab = struct
  type interface

  external interface_open': unit -> interface = "stub_gnttab_interface_open"

  let interface_open () =
    try
      interface_open' ()
    with e ->
      Printf.fprintf stderr "Failed to open grant table device: ENOENT\n";
      Printf.fprintf stderr "Does this system have Xen userspace grant table support?\n";
      Printf.fprintf stderr "On linux try:\n";
      Printf.fprintf stderr "  sudo modprobe xen-gntdev\n%!";
      raise e

  external interface_close: interface -> unit = "stub_gnttab_interface_close"

  type grant = {
    domid: domid;
    ref: gntref;
  }

  module Local_mapping = struct
    type t = {
      hs : grant_handle list;
      pages: Io_page.t;
    }

    let make hs pages = { hs; pages }

    let to_buf t = t.pages
  end

  (* There are 2 lowlevel API variants, which differ in whether they allocate
     the buffers for us or expect us to supply them. The userspace libxc will
     allocate internally. The raw kernelspace Mirage interface expects us to
     pass buffers in. *)

  external gnttab_allocates: unit -> bool = "stub_gnttab_allocates"
  let gnttab_allocates = gnttab_allocates ()

  external unmap_exn : interface -> grant_handle -> unit = "stub_gnttab_unmap"

  external map_onto_exn: interface -> gntref -> Io_page.t -> domid -> bool -> grant_handle = "stub_gnttab_map_onto"
  external map_fresh_exn: interface -> gntref -> domid -> bool -> (grant_handle * Io_page.t) = "stub_gnttab_map_fresh"

  let map_exn interface grant writable = match gnttab_allocates with
    | true ->
      let h, page = map_fresh_exn interface grant.ref grant.domid writable in
      Local_mapping.make [h] page
    | false ->
      let page = Io_page.get 1 in
      let h = map_onto_exn interface grant.ref page grant.domid writable in
      Local_mapping.make [h] page

  let map interface grant writable = try Some (map_exn interface grant writable) with _ -> None

  (* If the lowlevel API allocates then we must use a special mapv function to
     ensure the memory is mapped contiguously. *)
  external mapv_batched_exn: interface -> int array -> bool -> (grant_handle * Io_page.t) = "stub_gnttab_mapv_batched"

  let mapv_exn interface grants writable = match gnttab_allocates with
    | true ->
      let count = List.length grants in
      let grant_array = Array.make (count * 2) 0 in
      List.iteri (fun i g ->
          grant_array.(i * 2 + 0) <- g.domid;
          grant_array.(i * 2 + 1) <- g.ref;
        ) grants;
      let h, page = mapv_batched_exn interface grant_array writable in
      Local_mapping.make [h] page
    | false ->
      let nb_grants = List.length grants in
      let block = Io_page.get nb_grants in
      let pages = Io_page.to_pages block in
      let hs =
        List.fold_left2 (fun acc g p ->
            try (map_onto_exn interface g.ref p g.domid writable)::acc
            with exn ->
              List.iter (unmap_exn interface) acc;
              raise exn) [] grants pages
      in Local_mapping.make hs block

  let mapv interface gs p = try Some (mapv_exn interface gs p) with _ -> None

  let unmap_exn interface mapping = List.iter (unmap_exn interface) mapping.Local_mapping.hs

  let with_gnttab f =
    let intf = interface_open () in
    let result = try
        f intf
      with e ->
        interface_close intf;
        raise e
    in
    interface_close intf;
    result

  let with_mapping interface grant writable fn =
    let mapping = map interface grant writable in
    Lwt.finalize
      (fun () -> fn mapping)
      (fun () -> match mapping with
        | None -> Lwt.return ()
        | Some mapping -> Lwt.return (unmap_exn interface mapping)
      )
end

module Gntshr = struct
  type interface

  external gntshr_allocates: unit -> bool = "stub_gntshr_allocates"
  let gntshr_allocates = gntshr_allocates ()

  external interface_open': unit -> interface = "stub_gntshr_open"
  external interface_close: interface -> unit = "stub_gntshr_close"

  let interface_open () =
    try
      interface_open' ()
    with e ->
      Printf.fprintf stderr "Failed to open grant share device: ENOENT\n";
      Printf.fprintf stderr "Does this system have Xen userspace grant share support?\n";
      Printf.fprintf stderr "On linux try:\n";
      Printf.fprintf stderr "  sudo modprobe xen-gntalloc\n%!";
      raise e

  type share = {
    refs: gntref list;
    mapping: Io_page.t;
  }

  exception Interface_unavailable

  (* For kernelspace we need to track the real free grant table slots. *)

  let free_list : gntref Queue.t = Queue.create ()
  let free_list_waiters = Lwt_sequence.create ()

  let count_gntref = MProf.Counter.make ~name:"gntref"

  let put_no_count r =
    Queue.push r free_list;
    match Lwt_sequence.take_opt_l free_list_waiters with
    | None -> ()
    | Some u -> Lwt.wakeup u ()

  let put r =
    MProf.Counter.increase count_gntref (-1);
    put_no_count r

  let num_free_grants () = Queue.length free_list

  let rec get () =
    if gntshr_allocates
    then fail Interface_unavailable
    else match Queue.is_empty free_list with
      | true ->
        let th, u = MProf.Trace.named_task "Wait for free gnt" in
        let node = Lwt_sequence.add_r u free_list_waiters  in
        Lwt.on_cancel th (fun () -> Lwt_sequence.remove node);
        th >>= fun () -> get ()
      | false ->
        MProf.Counter.increase count_gntref (1);
        return (Queue.pop free_list)

  let get_n num =
    let rec gen_gnts num acc =
      match num with
      | 0 -> return acc
      | n ->
        begin
          get ()
          >>= fun gnt ->
          gen_gnts (n-1) (gnt :: acc)
        end
    in gen_gnts num []

  let get_nonblock () =
    if gntshr_allocates then raise Interface_unavailable;
    try Some (Queue.pop free_list) with Queue.Empty -> None

  let get_n_nonblock num =
    let rec aux acc num = match num with
      | 0 -> List.rev acc
      | n ->
        (match get_nonblock () with
         | Some p -> aux (p::acc) (n-1)
         (* If we can't have enough, we push them back in the queue. *)
         | None -> List.iter (fun gntref -> Queue.push gntref free_list) acc; [])
    in aux [] num

  let with_ref f =
    get ()
    >>= fun gnt ->
    Lwt.finalize
      (fun () -> f gnt)
      (fun () -> Lwt.return (put gnt))

  let with_refs n f =
    get_n n
    >>= fun gnts ->
    Lwt.finalize
      (fun () -> f gnts)
      (fun () -> Lwt.return (List.iter put gnts))

  external grant_access : gntref -> Io_page.t -> int -> bool -> unit = "stub_gntshr_grant_access"

  let grant_access ~domid ~writable gntref page =
    MProf.Trace.label "Gntshr.grant_access";
    if gntshr_allocates then raise Interface_unavailable;
    grant_access gntref page domid writable

  external end_access : gntref -> unit = "stub_gntshr_end_access"

  let end_access g =
    MProf.Trace.label "Gntshr.end_access";
    if gntshr_allocates then raise Interface_unavailable;
    end_access g

  let with_grant ~domid ~writable gnt page fn =
    grant_access ~domid ~writable gnt page;
    Lwt.finalize fn
      (fun () -> Lwt.return (end_access gnt))

  let with_grants ~domid ~writable gnts pages fn =
    Lwt.finalize
      (fun () ->
        List.iter (fun (gnt, page) ->
            grant_access ~domid ~writable gnt page) (List.combine gnts pages);
        fn ()
      ) (fun () ->
        Lwt.return (List.iter end_access gnts)
      )

  exception Need_xen_4_2_or_later
  let () = Callback.register_exception "gntshr.missing" Need_xen_4_2_or_later

  external share_pages_batched_exn: interface -> int -> int -> bool -> share = "stub_gntshr_share_pages_batched"

  (* XXX; we should block instead of failing! *)
  exception Grant_table_full

  let share_pages_individually_exn _ domid count writable =
    (* First allocate a list of n pages. *)
    let block = Io_page.get count in
    let pages = Io_page.to_pages block in
    let gntrefs = get_n_nonblock count in
    if gntrefs = []
    then raise Grant_table_full
    else begin
      List.iter2 (fun g p -> grant_access ~domid ~writable g p) gntrefs pages;
      { refs = gntrefs; mapping = block }
    end

  let share_pages_exn =
    if gntshr_allocates
    then share_pages_batched_exn
    else share_pages_individually_exn

  let share_pages interface domid count writable =
    try Some (share_pages_exn interface domid count writable)
    with _ -> None

  let munmap_individually_exn _ { refs; _ } =
    List.iter end_access refs

  external munmap_batched_exn: interface -> share -> unit = "stub_gntshr_munmap_batched"

  let munmap_exn =
    if gntshr_allocates
    then munmap_batched_exn
    else munmap_individually_exn

  let with_gntshr f =
    let intf = interface_open () in
    let result = try
        f intf
      with e ->
        interface_close intf;
        raise e
    in
    interface_close intf;
    result
end


external suspend : unit -> unit = "stub_gnttab_fini"

external init : unit -> unit = "stub_gnttab_init"

let resume () = init ()

external nr_entries : unit -> int = "stub_gnttab_nr_entries"
external nr_reserved : unit -> int = "stub_gnttab_reserved"

let _ =
  for i = nr_reserved () to nr_entries () - 1 do
    Gntshr.put_no_count i;
  done;
  init ()

