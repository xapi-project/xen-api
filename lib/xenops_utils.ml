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

let ( |> ) a b = b a

module D = Debug.Make(struct let name = "xenops_utils" end)
open D

module Unix = struct
  include Unix

  let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x
  let int_of_file_descr (x: Unix.file_descr) : int = Obj.magic x

  let file_descr_of_rpc x = x |> Rpc.int_of_rpc |> file_descr_of_int
  let rpc_of_file_descr x = x |> int_of_file_descr |> Rpc.rpc_of_int
end

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

module type ITEM = sig
  type t
  val t_of_rpc: Rpc.t -> t
  val rpc_of_t: t -> Rpc.t
  val namespace: string
  type key
  val key: key -> string list
end

(******************************************************************************)
(* Metadata storage                                                           *)

let root = ref None
let set_root service_name =
  root := Some ("/var/run/nonpersistent/" ^ service_name)
let get_root () = match !root with
  | None -> failwith "Xenops_utils.root not set"
  | Some x -> x

module StringMap = Map.Make(struct type t = string let compare = compare end)
type 'a fs =
  | Dir of 'a fs StringMap.t ref
  | Leaf of 'a

module type FS = sig
  val init: unit -> unit
  val mkdir: string list -> unit
  val read: string list -> Rpc.t option
  val write: string list -> Rpc.t -> unit
  val exists: string list -> bool
  val rm: string list -> unit
  val readdir: string list -> string list
end

(* Return all the non-empty prefixes of a given string, in descending order by length.
   prefixes_of [1; 2; 3] = [[1;2;3]; [1;2]; [1]] *)
let prefixes_of k =
  let prefixes, _ = List.fold_left
      (fun (acc, prefix) element ->
         (element :: prefix) :: acc, element :: prefix
      ) ([], []) k in
  List.map List.rev prefixes

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    Backtrace.is_important e;
    g ();
    raise e


let ignore_string (_: string) = ()
let ignore_bool (_: bool) = ()
let ignore_int (_: int) = ()

(* Recode an incoming string as valid UTF-8 *)
let utf8_recode str = 
  let out_encoding = `UTF_8 in
  let b = Buffer.create 1024 in
  let dst = `Buffer b in
  let src = `String str in
  let rec loop d e =
    match Uutf.decode d with 
    | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e 
    | `End -> ignore (Uutf.encode e `End)
    |`Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e 
    | `Await -> assert false
  in
  let d = Uutf.decoder src in 
  let e = Uutf.encoder out_encoding dst in
  loop d e;
  Buffer.contents b

module Mutex = struct
  include Mutex
  let execute m f =
    Mutex.lock m;
    finally f (fun () -> Mutex.unlock m)
end
module Opt = struct
  let default x = function
    | None -> x
    | Some x -> x
  let map f = function
    | None -> None
    | Some x -> Some (f x)
  let join = function
    | Some (Some a) -> Some a
    | _ -> None
  let iter f x = ignore (map f x)
  let to_list = function
    | Some x -> [x]
    | None -> []
  let unbox = function
    | Some x -> x
    | None -> raise Not_found
end
module List = struct
  include List
  let filter_map f x =
    List.fold_left (fun acc x -> match f x with
        | None -> acc
        | Some x -> x :: acc) [] x

end
module String = struct
  include String
  let startswith prefix x =
    String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)
end

module Date = struct
  type t = string
  let of_float x = 
    let time = Unix.gmtime x in
    Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
      (time.Unix.tm_year+1900)
      (time.Unix.tm_mon+1)
      time.Unix.tm_mday
      time.Unix.tm_hour
      time.Unix.tm_min
      time.Unix.tm_sec
  let to_string x = x
end
module Unixext = struct
  (** remove a file, but doesn't raise an exception if the file is already removed *)
  let unlink_safe file =
    try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

  (** create a directory but doesn't raise an exception if the directory already exist *)
  let mkdir_safe dir perm =
    try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  let mkdir_rec dir perm =
    let rec p_mkdir dir =
      let p_name = Filename.dirname dir in
      if p_name <> "/" && p_name <> "." 
      then p_mkdir p_name;
      try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
    p_mkdir dir

  (** daemonize a process *)
  let daemonize () =
    match Unix.fork () with
    | 0 ->
      if Unix.setsid () == -1 then
        failwith "Unix.setsid failed";

      begin match Unix.fork () with
        | 0 ->
          let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
          begin try
              Unix.close Unix.stdin;
              Unix.dup2 nullfd Unix.stdout;
              Unix.dup2 nullfd Unix.stderr;
            with exn -> Unix.close nullfd; raise exn
          end;
          Unix.close nullfd
        | _ -> exit 0
      end
    | _ -> exit 0

  exception Break

  let lines_fold f start input =
    let accumulator = ref start in
    let running = ref true in
    while !running do
      let line =
        try Some (input_line input)
        with End_of_file -> None
      in
      match line with
      | Some line ->
        begin
          try accumulator := (f !accumulator line)
          with Break -> running := false
        end
      | None ->
        running := false
    done;
    !accumulator

  (** open a file, and make sure the close is always done *)
  let with_input_channel file f =
    let input = open_in file in
    finally
      (fun () -> f input)
      (fun () -> close_in input)

  let with_file file mode perms f =
    let fd = Unix.openfile file mode perms in
    let r =
      try f fd
      with exn -> Unix.close fd; raise exn
    in
    Unix.close fd;
    r

  let file_lines_fold f start file_path = with_input_channel file_path (lines_fold f start)

  let file_lines_iter f = file_lines_fold (fun () line -> ignore(f line)) ()

  let readfile_line = file_lines_iter

  (** [fd_blocks_fold block_size f start fd] folds [f] over blocks (strings)
      		from the fd [fd] with initial value [start] *)
  let fd_blocks_fold block_size f start fd = 
    let block = String.create block_size in
    let rec fold acc = 
      let n = Unix.read fd block 0 block_size in
      (* Consider making the interface explicitly use Substrings *)
      let s = if n = block_size then block else String.sub block 0 n in
      if n = 0 then acc else fold (f acc s) in
    fold start

  let buffer_of_fd fd = 
    fd_blocks_fold 1024 (fun b s -> Buffer.add_string b s; b) (Buffer.create 1024) fd

  let buffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 buffer_of_fd

  let string_of_file file_path = Buffer.contents (buffer_of_file file_path)

  (** Makes a new file in the same directory as 'otherfile' *)
  let temp_file_in_dir otherfile =
    let base_dir = Filename.dirname otherfile in
    let rec keep_trying () =
      try
        let uuid =  Uuidm.to_string (Uuidm.create `V4) in
        let newfile = base_dir ^ "/" ^ uuid in
        Unix.close (Unix.openfile newfile [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_EXCL] 0o600);
        newfile
      with
        Unix.Unix_error (Unix.EEXIST, _, _)  -> keep_trying ()
    in keep_trying ()


  let atomic_write_to_file fname perms f =
    let tmp = Stdext.Filenameext.temp_file_in_dir fname in
    Unix.chmod tmp perms;
    finally
      (fun () ->
         let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT] perms (* ignored since the file exists *) in
         let result = finally
             (fun () -> f fd)
             (fun () -> Unix.close fd) in
         Unix.rename tmp fname; (* Nb this only happens if an exception wasn't raised in the application of f *)
         result)
      (fun () -> unlink_safe tmp)

  let write_string_to_file fname s =
    atomic_write_to_file fname 0o644 (fun fd ->
        let len = String.length s in
        let written = Unix.write fd s 0 len in
        if written <> len then (failwith "Short write occured!"))

  exception Process_still_alive

  let kill_and_wait ?(signal = Sys.sigterm) ?(timeout=10.) pid =
    let proc_entry_exists pid =
      try Unix.access (Printf.sprintf "/proc/%d" pid) [ Unix.F_OK ]; true
      with _ -> false
    in
    if pid > 0 && proc_entry_exists pid then (
      let loop_time_waiting = 0.03 in
      let left = ref timeout in
      let readcmdline pid =
        try string_of_file (Printf.sprintf "/proc/%d/cmdline" pid)
        with _ -> ""
      in
      let reference = readcmdline pid and quit = ref false in
      Unix.kill pid signal;

      (* We cannot do a waitpid here, since we might not be parent of
         			   the process, so instead we are waiting for the /proc/%d to go
         			   away. Also we verify that the cmdline stay the same if it's still here
         			   to prevent the very very unlikely event that the pid get reused before
         			   we notice it's gone *)
      while proc_entry_exists pid && not !quit && !left > 0.
      do
        let cmdline = readcmdline pid in
        if cmdline = reference then (
          (* still up, let's sleep a bit *)
          ignore (Unix.select [] [] [] loop_time_waiting);
          left := !left -. loop_time_waiting
        ) else (
          (* not the same, it's gone ! *)
          quit := true
        )
      done;
      if !left <= 0. then
        raise Process_still_alive;
    )

  let copy_file_internal ?limit reader writer =
    let buffer = String.make 65536 '\000' in
    let buffer_len = Int64.of_int (String.length buffer) in
    let finished = ref false in
    let total_bytes = ref 0L in
    let limit = ref limit in
    while not(!finished) do
      let requested = min (Opt.default buffer_len !limit) buffer_len in
      let num = reader buffer 0 (Int64.to_int requested) in
      let num64 = Int64.of_int num in

      limit := Opt.map (fun x -> Int64.sub x num64) !limit;
      ignore_int (writer buffer 0 num);
      total_bytes := Int64.add !total_bytes num64;
      finished := num = 0 || !limit = Some 0L;
    done;
    !total_bytes

  let copy_file ?limit ifd ofd = copy_file_internal ?limit (Unix.read ifd) (Unix.write ofd)

  let really_write fd string off n =
    let written = ref 0 in
    while !written < n do
      let wr = Unix.write fd string (off + !written) (n - !written) in
      written := wr + !written
    done
end

let dropnone x = List.filter_map (Opt.map (fun x -> x)) x

module FileFS = struct
  (** A directory tree containiign files, each of which contain strings *)

  let filename_of k = Printf.sprintf "%s/%s" (get_root ()) (String.concat "/" k)
  let paths_of k = List.map filename_of (prefixes_of k)

  let mkdir path = Unixext.mkdir_rec (filename_of path) 0o755
  let read path =
    try
      Some (filename_of path |> Unixext.string_of_file |> Jsonrpc.of_string)
    with e ->
      None
  let write path x =
    let filename = filename_of path in
    Unixext.mkdir_rec (Filename.dirname filename) 0o755;
    Unixext.write_string_to_file filename (Jsonrpc.to_string x)
  let exists path = Sys.file_exists (filename_of path)
  let rm path =
    List.iter
      (fun path ->
         debug "DB.delete %s" path;
         try
           if Unix.((lstat path).st_kind = S_DIR)
           then Unix.rmdir path
           else Unix.unlink path;
         with
         | Unix.Unix_error(Unix.ENOTEMPTY, _, _) ->
           (* This is thrown by the rmdir when we're trying to delete a directory
              				     that's got files still in it. It's not an error, so ignore it *)
           ()
         | Unix.Unix_error(Unix.ENOENT, _, _) ->
           (* This is thrown by the `lstat` if we're racing with another thread
              				     to delete a common directory. Not an error *)
           ()
         | e ->
           (* Anything else probably is an error, but we just log and continue *)
           error "Failed to DB.delete %s : %s" path (Printexc.to_string e);
           ()
      ) (paths_of path)
  let readdir path =
    let filename = filename_of path in
    if Sys.file_exists filename
    then Array.to_list (Sys.readdir filename)
    else []

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
    let rec aux path fs = match path, fs with
      | [], Dir d -> d
      | p :: ps, Dir d ->
        if StringMap.mem p !d
        then aux ps (StringMap.find p !d)
        else begin
          raise Not_dir
        end
      | _, Leaf _ -> begin
          raise Not_dir
        end in
    aux path !root

  let mkdir_locked path =
    List.iter
      (fun p ->
         let dir = dir_locked (dirname p) in
         if not(StringMap.mem (filename p) !dir)
         then dir := StringMap.add (filename p) (Dir(ref StringMap.empty)) !dir
      ) (List.rev (prefixes_of path))

  let mkdir path = Mutex.execute m (fun () -> mkdir_locked path)

  let read path =
    Mutex.execute m
      (fun () ->
         try
           match StringMap.find (filename path) !(dir_locked (dirname path)) with
           | Leaf x -> Some x
           | Dir _ -> None
         with _ -> None
      )

  let write path x =
    Mutex.execute m
      (fun () ->
         (* debug "DB.write %s <- %s" (String.concat "/" path) x; *)
         mkdir_locked (dirname path);
         let dir = dir_locked (dirname path) in
         dir := StringMap.add (filename path) (Leaf x) !dir
      )
  let exists path = Mutex.execute m (fun () -> try StringMap.mem (filename path) !(dir_locked (dirname path)) with _ -> false)
  let readdir path = Mutex.execute m (fun () -> try StringMap.fold (fun x _ acc -> x :: acc) !(dir_locked path) [] with _ -> [])
  let rm path =
    Mutex.execute m
      (fun () ->
         List.iter
           (fun p ->
              let dir = dir_locked (dirname p) in
              let deletable =
                if StringMap.mem (filename p) !dir
                then match StringMap.find (filename p) !dir with
                  | Dir child -> StringMap.is_empty !child
                  | Leaf _ -> true
                else false in
              if deletable then dir := StringMap.remove (filename p) !dir
           ) (prefixes_of path)
      )

  let init () = ()
end

let fs_backend = ref None
let get_fs_backend () = match !fs_backend with
  | Some x -> x
  | None -> failwith "No backend implementation set"

let set_fs_backend m =
  fs_backend := m;
  let module B = (val get_fs_backend () : FS) in
  B.init ()

module TypedTable = functor(I: ITEM) -> struct
  open I
  type key = string list
  let of_key k = I.namespace :: k
  let read (k: I.key) =
    let module FS = (val get_fs_backend () : FS) in
    let path = k |> I.key |> of_key in
    Opt.map (fun x -> t_of_rpc x) (FS.read path)
  let read_exn (k: I.key) = match read k with
    | Some x -> x
    | None -> raise (Does_not_exist (I.namespace, I.key k |> String.concat "/"))
  let write (k: I.key) (x: t) =
    let module FS = (val get_fs_backend () : FS) in
    let path = k |> I.key |> of_key in
    debug "TypedTable: Writing %s" (String.concat "/" path);
    FS.write path (rpc_of_t x)
  let exists (k: I.key) =
    let module FS = (val get_fs_backend () : FS) in
    FS.exists (k |> I.key |> of_key)
  let delete (k: I.key) =
    debug "TypedTable: Deleting %s" (k |> I.key |> of_key |> String.concat "/");
    let module FS = (val get_fs_backend () : FS) in
    FS.rm (k |> I.key |> of_key)

  let list (k: key) =
    let module FS = (val get_fs_backend () : FS) in
    FS.readdir (k |> of_key)

  let add (k: I.key) (x: t) =
    let path = k |> I.key |> of_key |> String.concat "/" in
    debug "TypedTable: Adding %s" path;
    if exists k then begin
      debug "Key %s already exists" path;
      raise (Already_exists(I.namespace, path))
    end else write k x

  let remove (k: I.key) =
    let path = k |> I.key |> of_key |> String.concat "/" in
    debug "TypedTable: Removing %s" path;
    if not(exists k) then begin	
      debug "Key %s does not exist" path;
      raise (Does_not_exist(I.namespace, path))
    end else delete k
end

(******************************************************************************)

let halted_vm = {
  Vm.power_state = Halted;
  domids = [];
  consoles = [];
  memory_target = 0L;
  memory_actual = 0L;
  memory_limit = 0L;
  vcpu_target = 0;
  rtc_timeoffset = "";
  uncooperative_balloon_driver = false;
  guest_agent = [];
  pv_drivers_detected = false;
  xsdata_state = [];
  last_start_time = 0.;
  shadow_multiplier_target = 1.;
  hvm = false;
  nomigrate=false;
  nested_virt=false;
  domain_type = Vm.Domain_undefined;
}

let unplugged_pci = {
  Pci.plugged = false;
}

let unplugged_vbd = {
  Vbd.active = false;
  plugged = false;
  qos_target = None;
  backend_present = None;
}

let unplugged_vif = {
  Vif.active = false;
  plugged = false;
  kthread_pid = 0;
  pvs_rules_active = false;
  media_present = false;
  device = None;
}

let unplugged_vgpu = {
  Vgpu.plugged = false;
  Vgpu.emulator_pid = None;
}

let unplugged_vusb = {
  Vusb.plugged = false;
}

let remap_vdi vdi_map = function 
  | Xenops_interface.VDI vdi -> 
    if List.mem_assoc vdi vdi_map 
    then (debug "Remapping VDI: %s -> %s" vdi (List.assoc vdi vdi_map); VDI (List.assoc vdi vdi_map))
    else VDI vdi 
  | x -> x 

let remap_vif vif_map vif =
  let open Xenops_interface in
  match vif.Vif.id with (_,device) ->
    if List.mem_assoc device vif_map
    then (debug "Remapping VIF: %s" device; {vif with Vif.backend = (List.assoc device vif_map)})
    else vif

let remap_vgpu vgpu_pci_map vgpu =
  match vgpu.Vgpu.id with (_,device) ->
    if List.mem_assoc device vgpu_pci_map
    then (debug "Remapping VGPU: %s" device; {vgpu with Vgpu.physical_pci_address = (List.assoc device vgpu_pci_map)})
    else vgpu

let strip x =
  if x.[String.length x - 1] = '\n'
  then String.sub x 0 (String.length x - 1)
  else x
let get_network_backend () =
  try
    Unixext.string_of_file !Resources.network_conf
    |>  strip
    |>  Stdext.Xstringext.String.split ' '
    |>  List.hd
  with _ ->
    failwith (Printf.sprintf "Failed to read network backend from: %s" !Resources.network_conf)

let _sys_hypervisor_type = "/sys/hypervisor/type"
let _sys_hypervisor_version_major = "/sys/hypervisor/version/major"
let _sys_hypervisor_version_minor = "/sys/hypervisor/version/minor"

type hypervisor =
  | Xen of string * string (* major, minor *)
  | Other of string

let detect_hypervisor () =
  if Sys.file_exists _sys_hypervisor_type then begin

    let hypervisor = strip (Unixext.string_of_file _sys_hypervisor_type) in
    match hypervisor with
    | "xen" ->
      let major = strip (Unixext.string_of_file _sys_hypervisor_version_major) in
      let minor = strip (Unixext.string_of_file _sys_hypervisor_version_minor) in
      Some (Xen(major, minor))
    | x -> Some (Other x)
  end else None

let chunks size lst =
  List.fold_left (fun acc op ->
      match acc with
      | [] -> [[op]]
      | xs::xss ->
        if List.length xs < size
        then (op::xs)::xss
        else [op]::xs::xss
    ) [] lst
  |> List.map (fun xs -> List.rev xs) |> List.rev
