(*
 * Copyright (c) Citrix Systems Inc.
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

let size = 128

let buffer : (int64 * Protocol.Event.t) option array = Array.make size None
let c = Lwt_condition.create ()

let next_id = ref 0L

let add event =
  let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
  buffer.(next_slot) <- Some (!next_id, event);
  next_id := Int64.succ !next_id;
  Lwt_condition.broadcast c ()

(* fold [f] over buffered items in chronological order *)
let fold f acc =
  let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
  let rec range start finish acc =
    if start > finish
    then acc
    else range (start + 1) finish (f buffer.(start) acc) in
  range 0 (next_slot - 1) (range next_slot (size - 1) acc)

let get from timeout : (int64 * Protocol.Event.t) list Lwt.t =
  let sleep = Lwt_unix.sleep timeout in
  let rec wait_for_data () =
    if !next_id <= from then
      Lwt_condition.wait ?mutex:(Some Switch_main_helper.m) c >>= wait_for_data
    else return ()
  in
  (* Wait until some data is available ie. when next_id > from (or timeout) *)
  Lwt.pick [ sleep; wait_for_data () ] >>= fun () ->
  (* start from next_slot, looking for non-None entries which
     	   are > from *)
  let reversed_results = fold (fun x acc -> match x with
      | None -> acc
      | Some (id, _) when id < from -> acc
      | Some (id, x) -> (id, x) :: acc) [] in
  return (List.rev reversed_results)
