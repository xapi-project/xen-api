(*
 * Copyright (C) Citrix Systems Inc.
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

open Xenops_interface
module Unixext = Xapi_stdext_unix.Unixext
module Mutex = Xapi_stdext_threads.Threadext.Mutex

let rpc_of ty x = Rpcmarshal.marshal ty.Rpc.Types.ty x

module D = Debug.Make (struct let name = "xenops_utils" end)

open D

module Unix = struct
  include Unix

  let file_descr_of_int (x : int) : Unix.file_descr = Obj.magic x

  let int_of_file_descr (x : Unix.file_descr) : int = Obj.magic x

  let file_descr_of_rpc x = x |> Rpc.int_of_rpc |> file_descr_of_int

  let rpc_of_file_descr x = x |> int_of_file_descr |> Rpc.rpc_of_int

  let typ_of_file_descr =
    Rpc.Types.Abstract
      {
        aname= "file_descr"
      ; test_data= [Unix.stdout]
      ; rpc_of= rpc_of_file_descr
      ; of_rpc= (fun x -> Ok (file_descr_of_rpc x))
      }
end

let all = List.fold_left ( && ) true

let any = List.fold_left ( || ) false

module type ITEM = sig
  type t

  val t : t Rpc.Types.def

  val namespace : string

  type key

  val key : key -> string list
end

(******************************************************************************)
(* Metadata storage                                                           *)

let root = ref None

let set_root service_name =
  root := Some ("/var/run/nonpersistent/" ^ service_name)

let get_root () =
  match !root with None -> failwith "Xenops_utils.root not set" | Some x -> x

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

type 'a fs = Dir of 'a fs StringMap.t ref | Leaf of 'a

module type FS = sig
  val init : unit -> unit

  val mkdir : string list -> unit

  val read : string list -> Rpc.t option

  val write : string list -> Rpc.t -> unit

  val exists : string list -> bool

  val rm : string list -> unit

  val readdir : string list -> string list

  val rename : string list -> string list -> unit
end

(* Return all the non-empty prefixes of a given string, in descending order by
   length. prefixes_of [1; 2; 3] = [[1;2;3]; [1;2]; [1]] *)
let prefixes_of k =
  let prefixes, _ =
    List.fold_left
      (fun (acc, prefix) element ->
        ((element :: prefix) :: acc, element :: prefix))
      ([], []) k
  in
  List.map List.rev prefixes

let ignore_string (_ : string) = ()

let ignore_bool (_ : bool) = ()

let ignore_int (_ : int) = ()

(* Recode an incoming string as valid UTF-8 *)
let utf8_recode str =
  let out_encoding = `UTF_8 in
  let b = Buffer.create 1024 in
  let dst = `Buffer b in
  let src = `String str in
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar _ as u ->
        ignore (Uutf.encode e u) ;
        loop d e
    | `End ->
        ignore (Uutf.encode e `End)
    | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep)) ;
        loop d e
    | `Await ->
        assert false
  in
  let d = Uutf.decoder src in
  let e = Uutf.encoder out_encoding dst in
  loop d e ; Buffer.contents b

let dropnone x = List.filter_map (Option.map Fun.id) x

module FileFS = struct
  (** A directory tree containiign files, each of which contain strings *)

  let filename_of k = Printf.sprintf "%s/%s" (get_root ()) (String.concat "/" k)

  let paths_of k = List.map filename_of (prefixes_of k)

  let mkdir path = Unixext.mkdir_rec (filename_of path) 0o755

  let read path =
    try Some (filename_of path |> Unixext.string_of_file |> Jsonrpc.of_string)
    with _ -> None

  let write path x =
    let filename = filename_of path in
    Unixext.mkdir_rec (Filename.dirname filename) 0o755 ;
    Unixext.write_string_to_file filename (Jsonrpc.to_string x)

  let exists path = Sys.file_exists (filename_of path)

  let rm path =
    List.iter
      (fun path ->
        debug "DB.delete %s" path ;
        try
          if Unix.((lstat path).st_kind = S_DIR) then
            Unix.rmdir path
          else
            Unix.unlink path
        with
        | Unix.Unix_error (Unix.ENOTEMPTY, _, _) ->
            (* This is thrown by the rmdir when we're trying to delete a
               directory that's got files still in it. It's not an error, so
               ignore it *)
            ()
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
            (* This is thrown by the `lstat` if we're racing with another thread
               to delete a common directory. Not an error *)
            ()
        | e ->
            (* Anything else probably is an error, but we just log and continue *)
            error "Failed to DB.delete %s : %s" path (Printexc.to_string e) ;
            ())
      (paths_of path)

  (** [rmtree path] removes a file or directory recursively without following
      symbolic links. It may raise [Failure] *)
  let rmtree path =
    let ( // ) = Filename.concat in
    let rec rm path =
      let st = Unix.lstat path in
      match st.Unix.st_kind with
      | Unix.S_DIR ->
          Sys.readdir path |> Array.iter (fun file -> rm (path // file)) ;
          Unix.rmdir path
      | _ ->
          Unix.unlink path
    in
    try rm path
    with exn ->
      let exn' = Printexc.to_string exn in
      let msg = Printf.sprintf "failed to remove %s: %s" path exn' in
      failwith msg

  let readdir path =
    let filename = filename_of path in
    if Sys.file_exists filename then
      Array.to_list (Sys.readdir filename)
    else
      []

  let rename path path' =
    let f = filename_of path in
    let f' = filename_of path' in
    Unixext.mkdir_rec (Filename.dirname f') 0o755 ;
    Unix.rename f f' ;
    List.iter (fun path -> try Unix.rmdir path with _ -> ()) (paths_of path)

  let init () = ()
end

module MemFS = struct
  (** An in-memory tree of Rpc.t values *)

  let root : Rpc.t fs ref = ref (Dir (ref StringMap.empty))

  let m = Mutex.create ()

  exception Not_dir

  exception Not_file

  let filename x = List.hd (List.rev x)

  let dirname x = List.rev (List.tl (List.rev x))

  (* return the Dir entry of a given path *)
  let dir_locked path =
    let rec aux path fs =
      match (path, fs) with
      | [], Dir d ->
          d
      | p :: ps, Dir d ->
          if StringMap.mem p !d then
            aux ps (StringMap.find p !d)
          else
            raise Not_dir
      | _, Leaf _ ->
          raise Not_dir
    in
    aux path !root

  let mkdir_locked path =
    List.iter
      (fun p ->
        let dir = dir_locked (dirname p) in
        if not (StringMap.mem (filename p) !dir) then
          dir := StringMap.add (filename p) (Dir (ref StringMap.empty)) !dir)
      (List.rev (prefixes_of path))

  let mkdir path = Mutex.execute m (fun () -> mkdir_locked path)

  let read path =
    Mutex.execute m (fun () ->
        try
          match StringMap.find (filename path) !(dir_locked (dirname path)) with
          | Leaf x ->
              Some x
          | Dir _ ->
              None
        with _ -> None)

  let write path x =
    Mutex.execute m (fun () ->
        (* debug "DB.write %s <- %s" (String.concat "/" path) x; *)
        mkdir_locked (dirname path) ;
        let dir = dir_locked (dirname path) in
        dir := StringMap.add (filename path) (Leaf x) !dir)

  let exists path =
    Mutex.execute m (fun () ->
        try StringMap.mem (filename path) !(dir_locked (dirname path))
        with _ -> false)

  let readdir path =
    Mutex.execute m (fun () ->
        try StringMap.fold (fun x _ acc -> x :: acc) !(dir_locked path) []
        with _ -> [])

  let rm path =
    Mutex.execute m (fun () ->
        List.iter
          (fun p ->
            let dir = dir_locked (dirname p) in
            let deletable =
              if StringMap.mem (filename p) !dir then
                match StringMap.find (filename p) !dir with
                | Dir child ->
                    StringMap.is_empty !child
                | Leaf _ ->
                    true
              else
                false
            in
            if deletable then dir := StringMap.remove (filename p) !dir)
          (prefixes_of path))

  let rename path path' =
    Mutex.execute m (fun () ->
        try
          let contents =
            match
              StringMap.find (filename path) !(dir_locked (dirname path))
            with
            | Leaf x ->
                x
            | Dir _ ->
                failwith
                  (Printf.sprintf
                     "Invalid: rename must be called on files not directories. \
                      %s is a directory"
                     (filename path))
          in
          let dir = dir_locked (dirname path') in
          dir := StringMap.add (filename path') (Leaf contents) !dir
        with _ -> ())

  let init () = ()
end

let fs_backend = ref None

let get_fs_backend () =
  match !fs_backend with
  | Some x ->
      x
  | None ->
      failwith "No backend implementation set"

let set_fs_backend m =
  fs_backend := m ;
  let module B = (val get_fs_backend () : FS) in
  B.init ()

module TypedTable =
functor
  (I : ITEM)
  ->
  struct
    open I

    type key = string list

    let of_key k = I.namespace :: k

    let get_path k = k |> I.key |> of_key

    (* Non-thread-safe functions: avoid calling these directly *)

    let write (k : I.key) (x : t) =
      let module FS = (val get_fs_backend () : FS) in
      let path = get_path k in
      debug "TypedTable: Writing %s" (String.concat "/" path) ;
      FS.write path (Rpcmarshal.marshal t.Rpc.Types.ty x)

    let delete (k : I.key) =
      debug "TypedTable: Deleting %s" (k |> I.key |> of_key |> String.concat "/") ;
      let module FS = (val get_fs_backend () : FS) in
      FS.rm (get_path k)

    (* Thread-safe functions *)

    let read (k : I.key) =
      let module FS = (val get_fs_backend () : FS) in
      let path = get_path k in
      Option.map
        (fun x ->
          match Rpcmarshal.unmarshal t.Rpc.Types.ty x with
          | Ok x ->
              x
          | Error (`Msg m) ->
              failwith
                (Printf.sprintf "Failed to unmarshal '%s': %s" I.namespace m))
        (FS.read path)

    let read_exn (k : I.key) =
      match read k with
      | Some x ->
          x
      | None ->
          raise
            (Xenopsd_error
               (Errors.Does_not_exist (I.namespace, I.key k |> String.concat "/")))

    let exists (k : I.key) =
      let module FS = (val get_fs_backend () : FS) in
      let path = get_path k in
      FS.exists path

    let list (k : key) =
      let module FS = (val get_fs_backend () : FS) in
      FS.readdir (k |> of_key)

    let rename (k : I.key) (k' : I.key) =
      let module FS = (val get_fs_backend () : FS) in
      FS.rename (get_path k) (get_path k')

    let m = Mutex.create ()

    let add (k : I.key) (x : t) =
      Mutex.execute m (fun () ->
          let path = k |> I.key |> of_key |> String.concat "/" in
          debug "TypedTable: Adding %s" path ;
          if exists k then (
            debug "Key %s already exists" path ;
            raise (Xenopsd_error (Errors.Already_exists (I.namespace, path)))
          ) else
            write k x)

    let remove (k : I.key) =
      Mutex.execute m (fun () ->
          let path = k |> I.key |> of_key |> String.concat "/" in
          debug "TypedTable: Removing %s" path ;
          if not (exists k) then (
            debug "Key %s does not exist" path ;
            raise (Xenopsd_error (Errors.Does_not_exist (I.namespace, path)))
          ) else
            delete k)

    (* The call `update k f` reads key `k` from the DB, passes the value to `f`,
       and updates the DB with the result. If the result is `None`, then the key
       will be deleted. Otherwise, its value will be modified. Returns a Boolean
       that indicates whether the operation actually changed what is in the DB.
       Note that `f` should never itself include an `add`, `remove` or another
       `update` or deadlock will occur! *)
    let update (k : I.key) (f : t option -> t option) : bool =
      Mutex.execute m (fun () ->
          let x = read k in
          let y = f x in
          (* Only update the DB if the value has changed *)
          if x <> y then (
            match y with
            | None ->
                delete k ; true
            | Some y' ->
                write k y' ; true
          ) else
            false)

    let rename (k : I.key) (k' : I.key) =
      Mutex.execute m (fun () ->
          let path = get_path k |> String.concat "/" in
          let path' = get_path k' |> String.concat "/" in
          assert (path <> path') ;
          debug "TypedTable: Renaming %s -> %s" path path' ;
          if exists k' then (
            debug "Key %s already exists" path' ;
            raise (Xenopsd_error (Errors.Already_exists (I.namespace, path')))
          ) ;
          if not (exists k) then (
            debug "Key %s does not exist" path ;
            raise (Xenopsd_error (Errors.Does_not_exist (I.namespace, path)))
          ) ;
          rename k k')

    (* Version of `update` that raises an exception if the read fails *)
    let update_exn (k : I.key) (f : t -> t option) =
      update k (function
        | None ->
            raise
              (Xenopsd_error
                 (Errors.Does_not_exist
                    (I.namespace, I.key k |> String.concat "/")))
        | Some x ->
            f x)
  end

(******************************************************************************)

let halted_vm =
  {
    Vm.power_state= Halted
  ; domids= []
  ; consoles= []
  ; memory_target= 0L
  ; memory_actual= 0L
  ; memory_limit= 0L
  ; vcpu_target= 0
  ; rtc_timeoffset= ""
  ; uncooperative_balloon_driver= false
  ; guest_agent= []
  ; pv_drivers_detected= false
  ; xsdata_state= []
  ; last_start_time= 0.
  ; shadow_multiplier_target= 1.
  ; hvm= false
  ; nomigrate= false
  ; nested_virt= false
  ; domain_type= Vm.Domain_undefined
  }

let unplugged_pci = {Pci.plugged= false}

let unplugged_vbd =
  {Vbd.active= false; plugged= false; qos_target= None; backend_present= None}

let unplugged_vif =
  {
    Vif.active= false
  ; plugged= false
  ; kthread_pid= 0
  ; pvs_rules_active= false
  ; media_present= false
  ; device= None
  }

let unplugged_vgpu =
  {Vgpu.active= false; Vgpu.plugged= false; Vgpu.emulator_pid= None}

let unplugged_vusb = {Vusb.plugged= false}

let remap_vdi vdi_map = function
  | Xenops_interface.VDI vdi ->
      if List.mem_assoc vdi vdi_map then (
        debug "Remapping VDI: %s -> %s" vdi (List.assoc vdi vdi_map) ;
        VDI (List.assoc vdi vdi_map)
      ) else
        VDI vdi
  | x ->
      x

let remap_vif vif_map vif =
  let open Xenops_interface in
  match vif.Vif.id with
  | _, device ->
      if List.mem_assoc device vif_map then (
        debug "Remapping VIF: %s" device ;
        {vif with Vif.backend= List.assoc device vif_map}
      ) else
        vif

let remap_vgpu vgpu_pci_map vgpu =
  let to_string addr = Pci.string_of_address addr in
  let _, pf_device = vgpu.Vgpu.id in
  let vf_device = "vf:" ^ pf_device in
  (* see infer_vgpu_map() in xapi *)
  let vgpu =
    match List.assoc_opt pf_device vgpu_pci_map with
    | None ->
        vgpu
    | Some addr ->
        debug "Remapping VGPU PF: %s -> %s" pf_device (to_string addr) ;
        {vgpu with Vgpu.physical_pci_address= addr}
  in
  let vgpu =
    match List.assoc_opt vf_device vgpu_pci_map with
    | None ->
        vgpu
    | Some addr ->
        debug "Remapping VGPU VF: %s -> %s" vf_device (to_string addr) ;
        {vgpu with Vgpu.virtual_pci_address= Some addr}
  in
  vgpu

let get_network_backend () =
  try String.trim (Unixext.string_of_file !Resources.network_conf)
  with _ ->
    failwith
      (Printf.sprintf "Failed to read network backend from: %s"
         !Resources.network_conf)

let _sys_hypervisor_type = "/sys/hypervisor/type"

let _sys_hypervisor_version_major = "/sys/hypervisor/version/major"

let _sys_hypervisor_version_minor = "/sys/hypervisor/version/minor"

type hypervisor =
  | Xen of string * string
  (* major, minor *)
  | Other of string

let detect_hypervisor () =
  if Sys.file_exists _sys_hypervisor_type then
    let hypervisor =
      String.trim (Unixext.string_of_file _sys_hypervisor_type)
    in
    match hypervisor with
    | "xen" ->
        let major =
          String.trim (Unixext.string_of_file _sys_hypervisor_version_major)
        in
        let minor =
          String.trim (Unixext.string_of_file _sys_hypervisor_version_minor)
        in
        Some (Xen (major, minor))
    | x ->
        Some (Other x)
  else
    None

let chunks size lst =
  List.fold_left
    (fun acc op ->
      match acc with
      | [] ->
          [[op]]
      | xs :: xss ->
          if List.length xs < size then
            (op :: xs) :: xss
          else
            [op] :: xs :: xss)
    [] lst
  |> List.map (fun xs -> List.rev xs)
  |> List.rev

let best_effort txt f =
  try f ()
  with e -> info "%s: ignoring exception %s" txt (Printexc.to_string e)

(* used only during startup *)
let json_length rpc = rpc |> Jsonrpc.to_string |> String.length

let xml_length rpc =
  let xml = rpc |> Xmlrpc.to_string in
  let length = ref (String.length xml) in
  (* protect_fn in xml_spaces in XAPI escapes some additional characters *)
  String.iter
    (function ' ' | '\t' | '\n' | '\r' | '%' -> incr length | _ -> ())
    xml ;
  !length

(* It is stored as Json in internal communication, but as xml in the database *)
let char_max_encoded_length =
  Array.init 256 (fun i ->
      if i >= 0x80 then
        3 (* Uutf.u_rep, see utf8_recode *)
      else
        let s = String.init 1 (fun _ -> Char.chr i) in
        let rpc = Rpc.String s in
        let empty = Rpc.String "" in
        max
          (json_length rpc - json_length empty)
          (xml_length rpc - xml_length empty))

let str_max_encoded_length str =
  let s = ref 0 in
  String.iter (fun c -> s := !s + char_max_encoded_length.(Char.code c)) str ;
  !s

let longest_encoded_char = Array.fold_left max 0 char_max_encoded_length

let entry_overhead = String.length "(''%.'')%."

let xenstore_encoded_entry_size_bytes key value =
  (* The largest amount of memory used by a xenstore-data entry:
     - in XAPI's database (encoded as an S-expression and then as XML)
     - in the JSON-RPC format

     Both of these have some constant per-entry overhead, e.g.:
        ('xenstore-key'%.'xenstore-value')%.('k2'%.'v2')...
        {"xenstore-key":"xenstore-value","k2":"v2"},...
     For calculating the overhead I used the S-expression encoding since that is larger.
     For individual bytes either the XML or JSON encoding could be larger,
     hence the `char_max_encoded_length` table above to determine the largest encoding among the two.
  *)
  entry_overhead + str_max_encoded_length key + str_max_encoded_length value
