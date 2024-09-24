(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Sexplib0.Sexp_conv

let my_name = ref (Filename.basename Sys.argv.(0))

let set_my_name x = my_name := x

module Mutex = struct
  include Mutex

  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock ;
    let r =
      begin try f () with exn -> Mutex.unlock lock ; raise exn
      end
    in
    Mutex.unlock lock ; r
end

type frame = {
    process: string
  ; filename: string
  ; line: int
  ; chars_start: int [@sexp.default 0]
  ; chars_end: int [@sexp.default 0]
  ; name: string option [@sexp.option]
  ; is_inline: bool [@sexp.default false]
  ; is_raise: bool [@sexp.default false]
}
[@@deriving sexp]

type t = frame list [@@deriving sexp]

let empty = []

(** [drop_wrapper str] drops anything before the last __.
    E.g `Dune_exe__Raiser` becomes `Raiser`,
  and `Xapi_database__Db_cache_impl` becomes `Db_cache_impl`.
  This makes backtraces easier to read, and the ambiguity introduced can be
  solved by looking at the filename and line number that is printed in the
  backtrace itself.
 *)
let drop_wrapper str =
  str
  |> Astring.String.cut ~rev:true ~sep:"__"
  |> Option.fold ~none:str ~some:snd

let to_string_hum xs =
  let xs' = List.length xs in
  let results = Buffer.create 10 in
  let rec loop first_line i = function
    | [] ->
        Buffer.contents results
    | x :: xs ->
        Printf.bprintf results "%d/%d %s " i xs' x.process ;
        Buffer.add_string results
          ( if first_line then
              "Raised at"
            else if x.is_raise then
              "Re-raised at"
            else
              "Called from"
          ) ;
        (* A standard OCaml stacktrace would look like this:
          Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
          Called from X.bar in file "x.ml" (inlined), line 2, characters 2-17
        *)
        Buffer.add_char results ' ' ;
        x.name
        |> Option.iter (fun name ->
            Buffer.add_string results (drop_wrapper name) ;
            Buffer.add_string results " in "
        ) ;
        Printf.bprintf results "file %S" x.filename ;
        if x.is_inline then Buffer.add_string results " (inlined)" ;
        Printf.bprintf results ", line %d" x.line ;
        if x.chars_start > 0 then
          Printf.bprintf results ", characters %d-%d" x.chars_start x.chars_end ;
        Buffer.add_char results '\n' ;
        loop false (i + 1) xs
  in
  match xs with
  | [] ->
      Printf.sprintf "%s: Thread %d has no backtrace table" !my_name
        Thread.(id (self ()))
  | _ ->
      loop true 1 xs

type table = {
    backtraces: t array
  ; exn_to_backtrace: exn Weak.t
  ; mutable producer: int (* free running counter *)
  ; m: Mutex.t
}

(* Increasing this makes 'find_all' slower and increases the amount of
   memory needed. Since we have a table per thread a small number should
   be enough. *)
let max_backtraces = 100

let frame_of_slot slot =
  let open Printexc in
  match Printexc.Slot.location slot with
  | None ->
      None
  | Some loc ->
      Some
        {
          process= !my_name
        ; filename= loc.filename
        ; line= loc.line_number
        ; chars_start= loc.start_char
        ; chars_end= loc.end_char
        ; name= Slot.name slot
        ; is_inline= Slot.is_inline slot
        ; is_raise= Slot.is_raise slot
        }

let frame_eq a b =
  a.process == b.process
  && a.line = b.line
  && a.chars_start = b.chars_start
  && a.chars_end = b.chars_end
  && String.equal a.filename b.filename

let[@tail_mod_cons] rec dedup_to_list' last xs =
  match xs () with
  | Seq.Nil ->
      []
  | Seq.Cons (x, xs) ->
      if frame_eq last x then
        dedup_to_list' x xs
      else
        x :: dedup_to_list' x xs

let dedup_to_list seq =
  match seq () with
  | Seq.Nil ->
      []
  | Seq.Cons (x, xs) ->
      x :: dedup_to_list' x xs

let frames_of_slots slots =
  slots |> Array.to_seq |> Seq.filter_map frame_of_slot |> dedup_to_list

let get_backtrace_411 () =
  Printexc.get_raw_backtrace ()
  |> Printexc.backtrace_slots
  |> Option.fold ~none:[] ~some:frames_of_slots

let make () =
  let backtraces = Array.make max_backtraces [] in
  let exn_to_backtrace = Weak.create max_backtraces in
  let producer = 0 in
  (* free running *)
  let m = Mutex.create () in
  {backtraces; exn_to_backtrace; producer; m}

let add t exn bt =
  Mutex.execute t.m (fun () ->
      let slot = t.producer mod max_backtraces in
      t.producer <- t.producer + 1 ;
      Weak.set t.exn_to_backtrace slot (Some exn) ;
      t.backtraces.(slot) <- bt
  )

let is_important t exn =
  let bt = get_backtrace_411 () in
  (* Deliberately clear the backtrace buffer *)
  (try raise Not_found with Not_found -> ()) ;
  add t exn bt

(* fold over the slots matching exn *)
let fold t exn f initial =
  let rec loop acc from =
    if from < 0 || t.producer - from > max_backtraces then
      acc
    else
      let slot = from mod max_backtraces in
      match Weak.get t.exn_to_backtrace slot with
      | Some exn' when exn' == exn ->
          loop (f acc slot) (from - 1)
      | _ ->
          loop acc (from - 1)
  in
  loop initial (t.producer - 1)

let remove_dups xs =
  List.fold_left
    (fun (last, acc) item ->
      ( item
      , if last = item then
          acc
        else
          item :: acc
      )
    )
    ([], []) (List.rev xs)
  |> snd

(*
  |> List.rev
*)
let get t exn =
  fold t exn (fun acc slot -> t.backtraces.(slot) :: acc) []
  |> remove_dups
  |> List.concat

let remove t exn =
  fold t exn
    (fun acc slot ->
      let bt = t.backtraces.(slot) in
      Weak.set t.exn_to_backtrace slot None ;
      t.backtraces.(slot) <- [] ;
      bt :: acc
    )
    []
  |> remove_dups
  |> List.concat

module IntMap = Map.Make (Int)

module ThreadLocalTable = struct
  (* The map values behave like stacks here, with shadowing as in Hashtbl.
     A Hashtbl is not used here, in order to avoid taking the lock in `find`. *)
  type 'a t = {mutable tbl: 'a list IntMap.t; m: Mutex.t}

  let make () =
    let tbl = IntMap.empty in
    let m = Mutex.create () in
    {tbl; m}

  let add t v =
    let id = Thread.(id (self ())) in
    Mutex.execute t.m (fun () ->
        t.tbl <-
          IntMap.update id
            (function Some v' -> Some (v :: v') | None -> Some [v])
            t.tbl
    )

  let remove t =
    let id = Thread.(id (self ())) in
    Mutex.execute t.m (fun () ->
        t.tbl <-
          IntMap.update id
            (function
              | Some [_] ->
                  None
              | Some (_hd :: tl) ->
                  Some tl
              | Some [] | None ->
                  None
              )
            t.tbl
    )

  let find t =
    let id = Thread.(id (self ())) in
    IntMap.find_opt id t.tbl
    |> Option.fold ~none:None ~some:(function v :: _ -> Some v | [] -> None)
end

let per_thread_backtraces = ThreadLocalTable.make ()

let ( let@ ) f x = f x

let try_result f = try Ok (f ()) with exn -> Error exn

let with_backtraces_common f with_table =
  let tbl =
    let tbl =
      match ThreadLocalTable.find per_thread_backtraces with
      | Some tbl ->
          tbl
      | None ->
          make ()
    in
    (* If we nest these functions we add multiple bindings
       to the same mutable table which is ok *)
    ThreadLocalTable.add per_thread_backtraces tbl ;
    tbl
  in
  let finally () = ThreadLocalTable.remove per_thread_backtraces in
  Fun.protect ~finally (fun () -> with_table tbl (try_result f))

module V1 = struct
  let with_backtraces f =
    let with_table tbl = function
      | Ok ok ->
          `Ok ok
      | Error e ->
          `Error (e, get tbl e)
    in
    with_backtraces_common f with_table
end

module V2 = struct
  let with_backtraces ~finally f =
    let with_table tbl result =
      result |> Result.map_error (function e -> (e, get tbl e)) |> finally
    in
    with_backtraces_common f with_table
end

let with_backtraces = V1.with_backtraces

let is_important exn =
  ThreadLocalTable.find per_thread_backtraces
  |> Option.iter (fun tbl -> is_important tbl exn)

let add exn bt =
  ThreadLocalTable.find per_thread_backtraces
  |> Option.iter (fun tbl -> add tbl exn bt)

let remove exn =
  ThreadLocalTable.find per_thread_backtraces
  |> Option.fold ~some:(fun tbl -> remove tbl exn) ~none:empty

let get exn =
  ThreadLocalTable.find per_thread_backtraces
  |> Option.fold ~some:(fun tbl -> get tbl exn) ~none:empty

let reraise old newexn =
  is_important old ;
  add newexn (remove old) ;
  raise newexn

module Interop = struct
  (* This matches xapi.py:exception *)
  type error = {
      error: string
    ; (* Python json.dumps and rpclib are not very friendly *)
      files: string list
    ; lines: int list
  }
  [@@deriving rpc]

  let of_json source_name txt =
    txt |> Jsonrpc.of_string |> error_of_rpc |> fun e ->
    List.combine e.files e.lines
    |> List.map (fun (filename, line) ->
        {
          process= source_name
        ; filename
        ; line
        ; chars_start= 0
        ; chars_end= 0
        ; name= None
        ; is_inline= false
        ; is_raise= false
        }
    )
end
