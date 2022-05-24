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

open Bos_setup

type def = Def of string * Rrd.cf_type

let name ~ds_name ~cf_type =
  cf_type
  |> Rrd.cf_type_to_string
  |> String.Ascii.lowercase
  |> Printf.sprintf "%s_%s" ds_name

let def ~data ~ds_name ~cf_type =
  let cfstr = Rrd.cf_type_to_string cf_type in
  let namestr = name ~ds_name ~cf_type in
  ( Def (ds_name, cf_type)
  , Printf.sprintf "DEF:%s=%s:%s:%s" namestr data ds_name cfstr
  )

let shape kind ~def:(Def (ds_name, cf_type)) ~r ~g ~b =
  let defstr = name ~ds_name ~cf_type in
  Printf.sprintf "%s:%s#%02x%02x%02x:%s(%s)" kind defstr r g b
    (Rrd.cf_type_to_string cf_type)
    ds_name

let area = shape "AREA"

let line = shape "LINE"

let rrdtool ~filename ~data title ~ds_name ~first ~last =
  let ds_min, def1 = def ~data ~ds_name ~cf_type:Rrd.CF_Min
  and ds_max, def2 = def ~data ~ds_name ~cf_type:Rrd.CF_Max
  and ds_avg, def3 = def ~data ~ds_name ~cf_type:Rrd.CF_Average in
  let graph =
    [
      def1
    ; def2
    ; def3
    ; area ~def:ds_min ~r:0 ~g:0xff ~b:0
    ; line ~def:ds_avg ~r:0 ~g:0 ~b:0xff
    ; line ~def:ds_max ~r:0xff ~g:0 ~b:0
    ]
  in
  Cmd.(
    v "rrdtool"
    % "graph"
    % "--imgformat"
    % "SVG"
    % filename
    % "--title"
    % title
    % "--width"
    % "1024"
    % "--height"
    % "368"
    % "--start"
    % Int64.to_string first
    % "--end"
    % Int64.to_string last
    %% of_list graph
  )

let process ~data rrd =
  let open Rrd in
  for rra_i = 0 to Array.length rrd.rrd_rras - 1 do
    let rra = rrd.rrd_rras.(rra_i) in
    let timespan =
      Int64.mul (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt)) rrd.timestep
    in
    let start = rrd.last_updated -. Int64.to_float timespan in
    Printf.printf "start=%f\n" start ;
    for j = 0 to Array.length rrd.rrd_dss - 1 do
      let ds_name = rrd.rrd_dss.(j).ds_name in
      Printf.printf "Doing ds: %s\n" ds_name ;
      let filename =
        Printf.sprintf "rrd_data_%s_%s_%Ld.svg" ds_name
          (cf_type_to_string rra.rra_cf)
          (Int64.mul
             (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt))
             rrd.timestep
          )
      in
      let title = ds_name (* TODO: better name? *) in
      let cmd =
        rrdtool ~data ~filename title ~ds_name ~first:(Int64.of_float start)
          ~last:(Int64.of_float rrd.last_updated)
      in
      OS.Cmd.run cmd
      |> Logs.on_error_msg ~use:(fun () -> failwith "rrdtool failed to run")
    done
  done

let finally f ~(always : unit -> unit) =
  match f () with
  | result ->
      always () ; result
  | exception e ->
      always () ; raise e

let with_input_file path f =
  if Fpath.has_ext "gz" path then
    let cmd = Cmd.(v "zcat" % p path) in
    let ic = cmd |> Cmd.to_string |> Unix.open_process_in in
    finally
      (fun () -> f ic)
      ~always:(fun () ->
        let (_ : Unix.process_status) = Unix.close_process_in ic in
        ()
      )
  else
    let ic = open_in Fpath.(to_string path) in
    finally (fun () -> f ic) ~always:(fun () -> close_in ic)

let with_input_rrd f filename =
  with_input_file filename @@ fun ic ->
  Logs.info (fun m -> m "Parsing RRD %a" Fpath.pp filename) ;
  let input = Xmlm.make_input (`Channel ic) in
  let rrd = Rrd.from_xml input in
  f ~filename rrd

type nicdir = RX | TX

let nicdir_of_string = function "rx" -> RX | "tx" -> TX | _ -> assert false

let string_of_nicdir = function RX -> "rx" | TX -> "tx"

let nicdir = Tyre.(conv nicdir_of_string string_of_nicdir @@ pcre "rx|tx")

type nickind = Data of nicdir | Errors of nicdir

let nickind_of_match = function
  | nicdir, None ->
      Data nicdir
  | nicdir, Some () ->
      Errors nicdir

let match_of_nickind = function
  | Data nicdir ->
      (nicdir, None)
  | Errors nicdir ->
      (nicdir, Some ())

let pp_nicdir = Fmt.(using string_of_nicdir string)

let pp_nickind ppf = function
  | Data nicdir ->
      Fmt.pf ppf "data %a" pp_nicdir nicdir
  | Errors nicdir ->
      Fmt.pf ppf "errors %a" pp_nicdir nicdir

let nicdir_or_errors =
  Tyre.(
    nicdir <&> opt @@ str "_errors" |> conv nickind_of_match match_of_nickind
  )

type category =
  | SR of {id: string; dsname: string}
  | CPU of {idx: int; cstate: int option}
  | CPUavg
  | NIC of {name: string; idx: int option; kind: nickind}
  | Tapdisks_lowmem
  | Memory_kib of {dsname: string}
  | Memory_reclaimed_bytes of {dsname: string option}
  | Pool_count of {dsname: string}
  | Xapi_kib of {dsname: string}
  | Openfds of {dsname: string}
  | Loadavg
  | Unknown of string

let make_sr (id, dsname) = SR {id; dsname}

let make_cpu (idx, cstate) = CPU {idx; cstate}

let make_nic ((name, idx), kind) = NIC {name; idx; kind}

let make_memory_kib dsname = Memory_kib {dsname}

let make_memory_bytes dsname = Memory_reclaimed_bytes {dsname}

let make_pool_count dsname = Pool_count {dsname}

let make_xapi_kib dsname = Xapi_kib {dsname}

let make_openfds () = Openfds {dsname= "xapi"}

let pp ppf = function
  (* TODO: load description from datasource list *)
  | SR {id; dsname} ->
      Fmt.pf ppf "SR (uuid=%s) %s" id dsname
  | CPU {idx; cstate} ->
      Fmt.pf ppf "CPU %d %a" idx Fmt.(option int) cstate
  | CPUavg ->
      Fmt.pf ppf "CPUavg"
  | NIC {name; idx; kind} ->
      Fmt.pf ppf "NIC %s%a %a" name Fmt.(option int) idx pp_nickind kind
  | Tapdisks_lowmem ->
      Fmt.pf ppf "Tapdisks_lowmem"
  | Memory_kib {dsname} ->
      Fmt.pf ppf "Memory %s (KiB)" dsname
  | Memory_reclaimed_bytes {dsname} ->
      Fmt.pf ppf "Memory_reclaimed %a (bytes)" Fmt.(option string) dsname
  | Pool_count {dsname} ->
      Fmt.pf ppf "pool_count %s" dsname
  | Xapi_kib {dsname} ->
      Fmt.pf ppf "XAPI memory %s kib" dsname
  | Openfds {dsname} ->
      Fmt.pf ppf "openfds %s" dsname
  | Loadavg ->
      Fmt.pf ppf "loadavg"
  | Unknown dsname ->
      Fmt.pf ppf "Unknown %s" dsname

let groups =
  let open Tyre in
  let uuid8 = pcre "[0-9a-f]{8}" in
  let uuid_rest = pcre "(-[0-9a-f]{4}){3}-[0-9a-f]{12}" in
  let dsname = pcre "[a-zA-Z_]+" in
  [
    (dsname <&> char '_' *> uuid8) --> make_sr
  ; (str "sr_" *> uuid8 <* uuid_rest <* char '_' <&> dsname) --> make_sr
  ; (str "cpu" *> int <&> opt @@ (str "-C" *> int)) --> make_cpu
  ; (str "cpu_avg" --> fun () -> CPUavg)
  ; (pcre "pif_" *> pcre "bond|eth|aggr"
    <&> opt int
    <* char '_'
    <&> nicdir_or_errors
    )
    --> make_nic
  ; (str "Tapdisks_in_low_memory_mode" --> fun () -> Tapdisks_lowmem)
  ; (str "memory_" *> dsname <* str "_kib") --> make_memory_kib
  ; (str "memory_reclaimed" *> opt dsname) --> make_memory_bytes
  ; (str "pool_" *> dsname <* str "_count") --> make_pool_count
  ; (str "xapi_" *> dsname <* str "_kib") --> make_xapi_kib
  ; str "xapi_open_fds" --> make_openfds
  ; (str "loadavg" --> fun () -> Loadavg)
  ]
  |> route

let pp_error ppf = function
  | `NoMatch (_re, str) ->
      Fmt.pf ppf "NoMatch '%s'" str
  | `ConverterFailure e ->
      Fmt.exn ppf e

let group name =
  let use _ = Unknown name in
  (* we have to shorten DS names to fit in RRD's 20 char limit *)
  Tyre.exec groups name |> Logs.on_error ~pp:pp_error ~use

let group_graphs rrd =
  let open Rrd in
  let names = ds_names rrd in
  names
  |> List.iter @@ fun name ->
     Logs.debug (fun m -> m "%s -> %a" name pp (group name))

type ds = Ds : string -> ds
(* to avoid mixing data source and filenames we use a different type here *)

let make_ds ?filename dsname =
  let dsname =
    if String.length dsname >= 20 then (
      Logs.warn (fun m ->
          m "RRD data source name exceeds 20 char limit: %s" dsname
      ) ;
      String.with_range dsname ~len:19
    ) else
      dsname
  in
  (Option.map Fpath.v filename, Ds dsname)

let make_sr (dsname, uuid) = make_ds ~filename:("_sr_" ^ uuid) dsname

let make_vbd (vbd, dsname) = make_ds ~filename:vbd dsname

let make_runstate dsname = make_ds ~filename:"runstate" dsname

(* top-level value to compile regexes only once *)
let classify =
  (* some RRD data source names are too long, max is 20 chars.
     Splitting RRDs into different files allows to shorten the names,
     e.g. remove the UUID from SR datasources.
     Some names are still too long, but those can be shortened without losing information. *)
  let open Tyre in
  let uuid8 = pcre "[0-9a-f]{8}" in
  let uuid_rest = pcre "(-[0-9a-f]{4}){3}-[0-9a-f]{12}" in
  let dsname = pcre "[a-zA-Z_]+" in
  let shorten from target = str from --> fun () -> make_ds target in
  [
    (dsname <&> char '_' *> uuid8) --> make_sr
  ; (str "sr_" *> uuid8 <* uuid_rest <* char '_' <&> dsname) --> make_sr
  ; shorten "Tapdisks_in_low_memory_mode" "Tapdisks_in_lowmem"
  ; ( (opt dsname <* str "memory_" <&> dsname) --> fun (pre, post) ->
      make_ds (Option.value ~default:"" pre ^ "mem_" ^ post)
    )
  ; (pcre "vbd_[^_]+" <* char '_' <&> dsname) --> make_vbd
  ; (str "runstate_" *> dsname) --> make_runstate
  ; ( (str "cpu" *> int <&> opt @@ (str "-C" *> int)) --> fun (cpuidx, cstate) ->
      let filename =
        match cstate with None -> "cpu" | Some n -> Printf.sprintf "cpu-C%d" n
      in
      make_ds ~filename (string_of_int cpuidx)
    )
  ; (pcre "pif_" *> dsname) --> make_ds ~filename:"pif"
    (* TODO: could provide info on polarity based on rx/tx and on kind, TICK for errors *)
  ]
  |> route

let classify_dsname dsname =
  let error _ = make_ds dsname in
  dsname |> Tyre.exec classify |> Result.fold ~ok:Fun.id ~error

type ds_def = {
    name_description: string option
  ; standard: bool
  ; min: float
  ; max: float
  ; units: string option
}

let default_def =
  {
    name_description= None
  ; standard= false
  ; min= Float.neg_infinity
  ; max= Float.infinity
  ; units= None
  }

module StringMap = Map.Make (String)

let classify ~ds_def ~filename ds =
  let open Rrd in
  let override, dsname = classify_dsname ds.ds_name in
  let pathname =
    let name = Fpath.rem_ext filename in
    match override with
    | None ->
        Fpath.(name + "_filtered")
    | Some newname ->
        Fpath.(name + to_string newname)
  in
  (* Logs.debug (fun m -> m "%s -> %a" ds.ds_name Fpath.pp pathname); *)
  let def =
    StringMap.find_opt ds.ds_name ds_def |> Option.value ~default:default_def
  in
  (* can only plot graphs with same units *)
  let extra =
    match def.units with
    | None ->
        (* use RRD type as approximation to "same unit", at least same kind of unit,
           e.g. rate vs duration *)
        Rrd.ds_type_to_string ds.ds_ty
    | Some u ->
        u
  in
  (Fpath.(pathname + extra |> add_ext "xml"), dsname)

let rrdtool =
  OS.Cmd.resolve (Cmd.v "rrdtool")
  |> Logs.on_error_msg ~use:(fun () -> failwith "rrdtool is not installed")

let rrd_restore filename rrd =
  let filename = Fpath.set_ext "xml" filename in
  Logs.debug (fun m -> m "Writing RRD xml to %a" Fpath.pp filename) ;
  Rrd_unix.to_file rrd (Fpath.to_string filename) ;
  let dot_rrd = Fpath.set_ext "rrd" filename in
  Logs.debug (fun m -> m "Restoring RRD to %a" Fpath.pp dot_rrd) ;
  Cmd.(rrdtool % "restore" % "-f" % p filename % p dot_rrd) |> OS.Cmd.run

(*let classify f seq =
  let fold map (group, item) = StringMap.add item group map in
  seq |> Seq.map f |> Seq.fold_left fold StringMap.empty *)

let split_rrd ~ds_def ~filename rrd =
  let open Rrd in
  let rrds = Hashtbl.create 3 in

  (* split the rrd into multiple rrds based on data source name *)
  let () =
    Logs.info (fun m -> m "classifying data sources") ;
    rrd.rrd_dss
    |> Array.iteri @@ fun i ds ->
       let filename, Ds ds_name = classify ~ds_def ~filename ds in
       let get_i rra = (rra.rra_data.(i), rra.rra_cdps.(i)) in
       let previous =
         Hashtbl.find_opt rrds filename |> Option.value ~default:[]
       in
       Hashtbl.replace rrds filename
       @@ (({ds with ds_name}, Array.map get_i rrd.rrd_rras) :: previous)
  in
  Logs.info (fun m -> m "Building and restoring RRDs") ;
  (* now build an RRD and restore it to binary .rrd form *)
  rrds
  |> Hashtbl.iter @@ fun filename lst ->
     Logs.debug (fun m -> m "Building %a" Fpath.pp filename) ;
     let rrd_dss, rrd_rras = List.split lst in
     let rrd_rras =
       rrd.rrd_rras
       |> Array.mapi @@ fun i rra ->
          let rra_seq = List.to_seq rrd_rras in
          let geti a = a.(i) in
          {
            rra with
            rra_data= rra_seq |> Seq.map geti |> Seq.map fst |> Array.of_seq
          ; rra_cdps= rra_seq |> Seq.map geti |> Seq.map snd |> Array.of_seq
          }
     in
     let rrd = {rrd with rrd_dss= Array.of_list rrd_dss; rrd_rras} in
     rrd_restore filename rrd
     |> Logs.on_error_msg ~use:(fun () -> failwith "Failed to restore RRD")

type mode = Split | Default | Plot

let parse_ds_def def k v =
  match k with
  | "name_description" when v <> "description not available" ->
      {def with name_description= Some v}
  | "standard" ->
      {def with standard= bool_of_string v}
  | "min" ->
      {def with min= float_of_string v}
  | "max" ->
      {def with max= float_of_string v}
  | "units" when v <> "unknown" ->
      {def with units= Some v}
  | _ ->
      def

let parse_ds_defs path =
  let fields line =
    line
    |> String.cut ~sep:":"
    |> Option.map @@ fun (k, v) -> (String.trim k, String.trim v)
  in
  let fold (map, key_opt) line =
    match (fields line, key_opt) with
    | Some ("name_label", ds_name), None ->
        (map, Some ds_name) (* start parsing new item *)
    | _, None ->
        (map, None) (* ignore *)
    | None, Some _ ->
        (map, None)
    | Some (k, v), Some ds_name ->
        let map =
          map
          |> StringMap.update ds_name @@ fun def ->
             Some (parse_ds_def (Option.value ~default:default_def def) k v)
        in
        (map, Some ds_name)
  in
  OS.File.fold_lines fold (StringMap.empty, None) path
  |> Logs.on_error_msg ~use:(fun _ ->
         failwith "Could not parse datasource definitions"
     )
  |> fst

let () =
  let open OS.Arg in
  let level =
    let conv =
      conv ~docv:"LEVEL" Logs.level_of_string
      @@ Fmt.of_to_string Logs.level_to_string
    in
    opt ["log"] conv ~absent:(Some Logs.Debug)
  in
  let mode =
    opt ["mode"] ~absent:Default @@ enum [("split", Split); ("plot", Plot)]
  in
  let data_source_list = opt ["def"] ~absent:None (some path) in
  let ds_def =
    Option.map parse_ds_defs data_source_list
    |> Option.value ~default:StringMap.empty
  in
  let paths = OS.Arg.(parse ~pos:path ()) in
  Logs.set_level level ;
  match mode with
  | Default ->
      let cmd =
        Cmd.(
          v "find" %% of_values p paths % "-name" % "*.gz" % "-print0"
          |> OS.Cmd.run_out
        )
      in
      (* TODO: forward level *)
      let xargs =
        Cmd.(
          v "xargs"
          % "-0"
          % "-P0"
          % "-n1"
          % Sys.executable_name
          % "--mode=split"
          |> OS.Cmd.run_in
        )
      in
      let res =
        OS.Cmd.out_run_in cmd
        |> Logs.on_error_msg ~use:(fun _ -> exit 1)
        |> xargs
      in
      Logs.on_error_msg ~use:(fun _ -> exit 1) res
  | Split ->
      paths |> List.iter @@ with_input_rrd (split_rrd ~ds_def)
  | Plot ->
      failwith "TODO"
