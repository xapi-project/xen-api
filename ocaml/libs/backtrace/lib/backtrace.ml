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

type frame = {process: string; filename: string; line: int} [@@deriving sexp]

type t = frame list [@@deriving sexp]

let empty = []

let to_string_hum xs =
  let xs' = List.length xs in
  let results = Buffer.create 10 in
  let rec loop first_line i = function
    | [] ->
        Buffer.contents results
    | x :: xs ->
        Buffer.add_string results
          (Printf.sprintf "%d/%d %s %s file %s, line %d" i xs' x.process
             ( if first_line then
                 "Raised at"
               else
                 "Called from"
             )
             x.filename x.line
          ) ;
        Buffer.add_string results "\n" ;
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

let frame_of_location loc =
  let open Printexc in
  {process= !my_name; filename= loc.filename; line= loc.line_number}

let frame_of_slot slot =
  slot |> Printexc.Slot.location |> Option.map frame_of_location

let frame_eq a b =
  a.process == b.process
  && a.line = b.line
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

let get_backtrace_402 () =
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
  let bt = get_backtrace_402 () in
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
    |> List.map (fun (filename, line) -> {process= source_name; filename; line})
end
