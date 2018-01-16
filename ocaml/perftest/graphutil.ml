(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Xapi_stdext_std
open Xapi_stdext_unix
open Testtypes
open Perfdebug

type short_info = string * string * string
type info = short_info * float list

let merge_infos (infos:info list) =
  let names = Listext.List.setify (List.map (fun ((file,result,subtest),_) -> (file,result,subtest)) infos) in
  let floats ((file,result,subtest) as i)= i, List.flatten (List.map (fun ((f,r,s),fl) -> if file=f && result=r && subtest=s then fl else []) infos) in
  let merge_infos = List.map floats names in
  debug "Available data:";
  List.iter (fun ((f,r,s),fl) -> debug "\t* in file: %s \t%s \t%s \t-- %i points" f r s (List.length fl)) merge_infos;
  merge_infos

let clone_cnt = ref 0

let info_from_raw_result ?(separate=false) file result : info list =
  match result.rawresult with
  | StartTest floats | ShutdownTest floats -> [ (file, result.resultname, result.subtest), floats]
  | CloneTest floats ->
    (* Pretend that we got the data from separate files, so they are considered as separate data series *)
    let file = Printf.sprintf "%s-%d" file !clone_cnt in
    (* Make the resultnames distinct to force the lines onto separate graphs *)
    let resultname = if separate then (Printf.sprintf "%s-%d" result.resultname !clone_cnt) else result.resultname in
    let subtest = result.subtest in
    clone_cnt := !clone_cnt+1;
    [ (file,resultname,subtest), floats ]
  | _ -> []

let floats_from_file fname =
  let floats = ref [] in
  Unixext.readfile_line (fun line -> floats := float_of_string (String.trim line) :: !floats) fname;
  !floats

let get_info ?(separate=false) files : info list =
  let aux f =
    match Testtypes.from_string (Unixext.string_of_file f) with
    | None -> [ (f, "", ""), floats_from_file f]
    | Some results -> List.flatten (List.map (info_from_raw_result ~separate f) results)
  in
  merge_infos (List.flatten (List.map aux files))

let short_info_to_string ((file,result,subtest) : short_info) =
  Printf.sprintf "%s.%s.%s" result subtest file

let short_info_to_title ((_,_,subtest) : short_info) = subtest

let get_result ((_,result,_):short_info) = result

let get_result_types (all_info:info list) =
  Listext.List.setify (List.map (fun ( (_,result,_),_) -> result) all_info)

let replace_assoc r n l =
  if List.mem_assoc r l
  then (r,n) :: (List.remove_assoc r l)
  else (r,n) :: l

let get_op op extremum (infos:info list) =
  let mem : (string * float) list ref = ref [] in
  let aux ((_,result,_),floats) =
    if List.mem_assoc result !mem
    then mem := (result, List.fold_left op (List.assoc result !mem) floats) :: (List.remove_assoc result !mem)
    else mem := (result, List.fold_left op extremum floats) :: !mem
  in
  List.iter aux infos;
  !mem

let get_min = get_op min max_float
let get_max = get_op max min_float

let string_of_result = function
  | "startall"          -> "sequential VM.start"
  | "stopall"           -> "sequential VM.stop"
  | "parallel_startall" -> "parallel VM.start"
  | "parallel_stopall"  -> "parallel VM.stop"
  | "clone"             -> "parallel VM.clone"
  | s when (Xstringext.String.startswith "clone-" s) -> "parallel VM.clone"
  | _ -> "???"
