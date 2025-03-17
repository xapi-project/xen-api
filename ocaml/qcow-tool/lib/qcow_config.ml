(*
 * Copyright (C) 2017 David Scott <dave@recoil.org>
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
 *
 *)

type t = {
  id: string;
  discard: bool;
  keep_erased: int64 option;
  compact_after_unmaps: int64 option;
  check_on_connect: bool;
  runtime_asserts: bool;
  read_only: bool;
}
let fresh_id =
  let id = ref 0 in
  fun () ->
    let result = "unknown_" ^ (string_of_int !id) in
    incr id;
    result
let create ?(id = fresh_id ()) ?(discard=false) ?keep_erased ?compact_after_unmaps ?(check_on_connect=true) ?(runtime_asserts=false) ?(read_only=false) () =
  { id; discard; keep_erased; compact_after_unmaps; check_on_connect; runtime_asserts; read_only }
let to_string t = Printf.sprintf "id=%s;discard=%b;keep_erased=%scompact_after_unmaps=%s;check_on_connect=%b;runtime_asserts=%b;read_only=%b"
    t.id t.discard
    (match t.keep_erased with None -> "0" | Some x -> Int64.to_string x)
    (match t.compact_after_unmaps with None -> "0" | Some x -> Int64.to_string x)
    t.check_on_connect t.runtime_asserts t.read_only
let default () = { id = fresh_id (); discard = false; keep_erased = None; compact_after_unmaps = None; check_on_connect = true; runtime_asserts = false; read_only = false }
let of_string txt =
  let open Astring in
  try
    let strings = String.cuts ~sep:";" txt in
    Ok (List.fold_left (fun t line ->
        match String.cut ~sep:"=" line with
        | None -> t
        | Some (k, v) ->
          begin match String.Ascii.lowercase k with
            | "id" -> { t with id = v }
            | "discard" -> { t with discard = bool_of_string v }
            | "keep_erased" ->
              let keep_erased = if v = "0" then None else Some (Int64.of_string v) in
              { t with keep_erased }
            | "compact_after_unmaps" ->
              let compact_after_unmaps = if v = "0" then None else Some (Int64.of_string v) in
              { t with compact_after_unmaps }
            | "check_on_connect" -> { t with check_on_connect = bool_of_string v }
            | "runtime_asserts" -> { t with runtime_asserts = bool_of_string v }
            | "read_only" -> { t with read_only = bool_of_string v }
            | x -> failwith ("Unknown qcow configuration key: " ^ x)
          end
      ) (default ()) strings)
  with
  | e -> Error (`Msg (Printexc.to_string e))
