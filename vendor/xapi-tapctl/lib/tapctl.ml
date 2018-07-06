open Forkhelpers
open Xapi_stdext_unix
open Xapi_stdext_std.Xstringext
open Xapi_stdext_std.Listext
open Xapi_stdext_threads.Threadext
open Xapi_stdext_monadic

(* Tapdisk stats *)
module Stats = struct
  module Tap = struct
    type t = {
      minor: int;
      reqs: (int64 * int64);
      kicks: (int64 * int64);
    } [@@deriving rpc]
  end

  module Driver = struct
    type t = {
      ty : int;
      name : string;
    } [@@deriving rpc]

    let rpc_of_t x = match (rpc_of_t x) with | Rpc.Dict x -> Rpc.Dict (List.map (function ("ty",y) -> ("type",y) | x -> x) x) | y -> y
    let t_of_rpc rpc = t_of_rpc (match rpc with | Rpc.Dict x -> Rpc.Dict (List.map (function ("type",y) -> ("ty",y) | x -> x) x ) | y -> y)

  end

  module Image = struct
    type t = {
      name : string;
      hits : int64 * int64;
      fail : int64 * int64;
      driver : Driver.t
    } [@@deriving rpc]
  end

  type t = {
    name : string;
    secs : (int64 * int64);
    images : Image.t list;
    tap : Tap.t;
    nbd_mirror_failed : int;
    reqs_outstanding : int;
  } [@@deriving rpc]
end




type tapdev = {
  minor : int;
  tapdisk_pid : int;
} [@@deriving rpc]

type t = tapdev * string * (string * string) option


type context = {
  host_local_dir: string;
  dummy: bool;
}

let create () = { host_local_dir = ""; dummy = false }
let create_dummy dir =
  {host_local_dir=dir; dummy=true }

let get_devnode_dir ctx =
  let d = Printf.sprintf "%s/dev/xen/blktap-2" ctx.host_local_dir in
  Unixext.mkdir_rec d 0o755;
  d
let get_blktapstem ctx = Printf.sprintf "%s/dev/xen/blktap-2/blktap" ctx.host_local_dir
let get_tapdevstem ctx = Printf.sprintf "%s/dev/xen/blktap-2/tapdev" ctx.host_local_dir

type driver = | Vhd | Aio

let string_of_driver = function
  | Vhd -> "vhd"
  | Aio -> "aio"

(* DUMMY MODE FUNCTIONS *)

let get_minor tapdev = tapdev.minor
let get_tapdisk_pid tapdev = tapdev.tapdisk_pid

module Dummy = struct
  type dummy_tap = {
    d_minor : int option;
    d_pid : int option;
    d_state : string option;
    d_args : string option;
  } and dummy_tap_list = dummy_tap list [@@deriving rpc]

  let d_lock = Mutex.create ()

  let get_dummy_tapdisk_list_filename ctx =
    let file = Printf.sprintf "%s/dev/tapdisks" ctx.host_local_dir in
    Unixext.mkdir_rec (Filename.dirname file) 0o777;
    file

  let get_dummy_tapdisk_list ctx =
    let filename = get_dummy_tapdisk_list_filename ctx in
    try
      dummy_tap_list_of_rpc (Jsonrpc.of_string (Unixext.string_of_file filename))
    with _ -> []

  let write_dummy_tapdisk_list ctx list =
    let filename = get_dummy_tapdisk_list_filename ctx in
    let str = Jsonrpc.to_string (rpc_of_dummy_tap_list list) in
    Unixext.write_string_to_file filename str

  let find_next_unused_number list =
    if List.length list = 0 then 0 else
      let list_plus_one = List.map ((+) 1) list in
      let diff = List.set_difference list_plus_one list in
      List.hd diff

  let find_next_unused_minor list =
    let minors = List.filter_map (fun t -> t.d_minor) list in
    find_next_unused_number minors

  let find_next_unused_pid list =
    let pids = List.filter_map (fun t -> t.d_pid) list in
    find_next_unused_number pids

  let get_entry_from_pid pid list =
    try Some (List.find (fun entry -> entry.d_pid = Some pid) list) with _ -> None

  let get_entry_from_minor minor list =
    try Some (List.find (fun entry -> entry.d_minor = Some minor) list) with _ -> None

  let allocate ctx =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let minor = find_next_unused_minor list in
        let entry = {
          d_minor = Some minor;
          d_pid = None;
          d_state = None;
          d_args = None;
        } in
        let stem = get_tapdevstem ctx in
        let dummy_device = Printf.sprintf "%s%d" stem minor in
        Unixext.unlink_safe dummy_device;
        Unixext.touch_file dummy_device;
        write_dummy_tapdisk_list ctx (entry::list);
        minor
      )

  let spawn ctx =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let pid = find_next_unused_pid list in
        let entry = {
          d_minor = None;
          d_pid = Some pid;
          d_state = None;
          d_args = None;
        } in
        write_dummy_tapdisk_list ctx (entry::list);
        pid
      )

  let attach ctx pid minor =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        begin (* sanity check *)
          match (get_entry_from_pid pid list, get_entry_from_minor minor list) with
          | Some e1, Some e2 ->
            if e1.d_minor <> None then failwith "pid already attached!";
            if e2.d_pid <> None then failwith "minor already in use!";
          | None, Some _ ->
            failwith "pid nonexistant"
          | Some _, None ->
            failwith "minor nonexistant"
          | None, None ->
            failwith "neither pid nor minor exist!"
        end;
        let new_entry = {
          d_minor = Some minor;
          d_pid = Some pid;
          d_state = Some "0";
          d_args = None;
        } in
        let list = List.filter (fun e -> e.d_pid <> Some pid && e.d_minor <> Some minor) list in
        write_dummy_tapdisk_list ctx (new_entry::list);
        {tapdisk_pid=pid; minor=minor})

  let _open ctx t leaf_path driver =
    let args = Printf.sprintf "%s:%s" (string_of_driver driver) leaf_path in
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let list = List.map (fun e ->
            if e.d_pid = Some t.tapdisk_pid && e.d_minor = Some t.minor
            then { e with
                   d_state = Some "0";
                   d_args = Some args }
            else e) list in
        write_dummy_tapdisk_list ctx list)

  let close ctx t =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let list = List.map (fun e ->
            if e.d_pid = Some t.tapdisk_pid && e.d_minor = Some t.minor
            then { e with
                   d_state = Some "0x2";
                   d_args = None }
            else e) list in
        write_dummy_tapdisk_list ctx list)

  let pause ctx t =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let list = List.map (fun e ->
            if e.d_pid = Some t.tapdisk_pid && e.d_minor = Some t.minor
            then { e with d_state = Some "0x2a" }
            else e) list in
        write_dummy_tapdisk_list ctx list)

  let unpause ctx t leaf_path driver =
    let args = Printf.sprintf "%s:%s" (string_of_driver driver) leaf_path in
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let list = List.map (fun e ->
            if e.d_pid = Some t.tapdisk_pid && e.d_minor = Some t.minor
            then { e with
                   d_state = Some "0";
                   d_args = Some args }
            else e) list in
        write_dummy_tapdisk_list ctx list)

  let detach ctx t =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let (a,b) = get_entry_from_pid t.tapdisk_pid list, get_entry_from_minor t.minor list in
        if a<>None && a <> b then failwith "Not attached";
        let list = List.filter (fun entry -> entry.d_pid <> Some t.tapdisk_pid) list in
        let list = { d_minor = Some t.minor;
                     d_pid = None;
                     d_state = None;
                     d_args = None; }::list in
        write_dummy_tapdisk_list ctx list)

  let free ctx minor =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        let entry = get_entry_from_minor minor list in
        begin (* sanity check *)
          match entry with
          | Some e -> if e.d_pid <> None then failwith "Can't free an attached minor"
          | None -> failwith "Unknown minor"
        end;
        let list = List.filter (fun e -> e.d_minor <> Some minor) list in
        write_dummy_tapdisk_list ctx list)

  let list ?t ctx =
    Mutex.execute d_lock (fun () ->
        let list = get_dummy_tapdisk_list ctx in
        List.filter_map (fun e ->
            let args =
              match Opt.map (String.split ':') e.d_args with
              | Some (ty::arguments) ->
                Some (ty,String.concat ":" arguments)
              | _ -> None
            in
            match (e.d_minor, e.d_pid, e.d_state, t) with
            | Some m, Some p, Some s, None ->
              Some ({tapdisk_pid=p; minor=m},s,args)
            | Some m, Some p, Some s, Some t ->
              if t.tapdisk_pid = p && t.minor=m then
                Some ({tapdisk_pid=p; minor=m},s,args)
              else
                None
            | _ -> None) list)

  let stats ctx t =
    let open Stats in
    { name = "none";
      secs = 0L,0L;
      images = [];
      tap = { Tap.minor = t.minor;
              reqs = 0L,0L;
              kicks = 0L,0L; };
      nbd_mirror_failed = 0;
      reqs_outstanding = 0;
    }
end


(* END OF DUMMY STUFF *)

let colon = Re_str.regexp_string ":"

let canonicalise x =
  if not(Filename.is_relative x)
  then x
  else begin
    (* Search the PATH and XCP_PATH for the executable *)
    let paths = Re_str.split colon (Sys.getenv "PATH") in
    let xen_paths = try Re_str.split colon (Sys.getenv "XCP_PATH") with _ -> [] in
    let first_hit = List.fold_left (fun found path -> match found with
        | Some hit -> found
        | None ->
          let possibility = Filename.concat path x in
          if Sys.file_exists possibility
          then Some possibility
          else None
      ) None (xen_paths @ paths) in
    match first_hit with
    | None -> x
    | Some hit -> hit
  end

let tap_ctl = canonicalise "tap-ctl"

let invoke_tap_ctl ctx cmd args =
  let find x = try [ x ^ "=" ^ (Sys.getenv x) ] with _ -> [] in
  let env = Array.of_list (find "PATH" @ (find "TAPDISK") @ (find "TAPDISK2")) in
  let stdout, stderr = execute_command_get_output ~env tap_ctl (cmd::args) in
  stdout

let allocate ctx =
  if ctx.dummy then Dummy.allocate ctx else begin
    let result = invoke_tap_ctl ctx "allocate" [] in
    let stem = get_tapdevstem ctx in
    let stemlen = String.length stem in
    assert(String.startswith stem result);
    let minor_str = (String.sub result stemlen (String.length result - stemlen)) in
    let minor = Scanf.sscanf minor_str "%d" (fun d -> d) in
    minor
  end

let devnode ctx minor =
  Printf.sprintf "%s%d" (get_tapdevstem ctx) minor

let spawn ctx =
  if ctx.dummy then Dummy.spawn ctx else begin
    let result = invoke_tap_ctl ctx "spawn" [] in
    let pid = Scanf.sscanf result "%d" (fun d -> d) in
    pid
  end

let attach ctx pid minor =
  if ctx.dummy then Dummy.attach ctx pid minor else begin
    let _ = invoke_tap_ctl ctx "attach" ["-p"; string_of_int pid; "-m"; string_of_int minor] in
    {minor=minor; tapdisk_pid=pid}
  end

let args tapdev =
  ["-p"; string_of_int tapdev.tapdisk_pid; "-m"; string_of_int tapdev.minor]

let _open ctx t leaf_path driver =
  if ctx.dummy then Dummy._open ctx t leaf_path driver else begin
    ignore(invoke_tap_ctl ctx "open" (args t @ ["-a"; Printf.sprintf "%s:%s" (string_of_driver driver) leaf_path]))
  end

let close ctx t =
  if ctx.dummy then Dummy.close ctx t else begin
    ignore(invoke_tap_ctl ctx "close" (args t))
  end
let pause ctx t =
  if ctx.dummy then Dummy.pause ctx t else begin
    ignore(invoke_tap_ctl ctx "pause" (args t))
  end

let unpause ctx t leaf_path driver =
  if ctx.dummy then Dummy.unpause ctx t leaf_path driver else begin
    ignore(invoke_tap_ctl ctx "unpause" (args t @ [ "-a"; Printf.sprintf "%s:%s" (string_of_driver driver) leaf_path ]))
  end

let detach ctx t =
  if ctx.dummy then Dummy.detach ctx t else begin
    ignore(invoke_tap_ctl ctx "detach" (args t))
  end

let free ctx minor =
  if ctx.dummy then Dummy.free ctx minor else begin
    ignore(invoke_tap_ctl ctx "free" ["-m"; string_of_int minor])
  end

let list ?t ctx =
  if ctx.dummy then Dummy.list ?t ctx else begin
    let args = match t with
      | Some tapdev -> args tapdev
      | None -> []
    in
    let result = invoke_tap_ctl ctx "list" args in
    let lines = String.split '\n' result in
    List.filter_map (fun line ->
        (* Note the filename can include spaces, for example: *)
        (* pid=855 minor=0 state=0 args=aio:/run/sr-mount/dev/disk/by-id/ata-WDC_WD2502ABYS-18B7A0_WD-WCAT1H334077-part3/win8 0 *)
        try
          let fields = String.split ~limit:4 ' ' line in
          let assoc = List.filter_map (fun field ->
              match String.split '=' field with
              | x::ys ->
                Some (x,String.concat "=" ys)
              | _ ->
                None) fields
          in
          let args =
            try
              match String.split ':' (List.assoc "args" assoc) with
              | ty::arguments ->
                Some (ty,String.concat ":" arguments)
              | _ -> None
            with _ -> None
          in
          Some ({tapdisk_pid=int_of_string (List.assoc "pid" assoc); minor=int_of_string (List.assoc "minor" assoc)},(List.assoc "state" assoc),args)
        with _ -> None) lines
  end

let is_paused ctx t =
  let result = list ~t ctx in
  match result with
  | [(tapdev,state,args)] -> state="0x2a"
  | _ -> failwith "Unknown device"

let is_active ctx t =
  let result = list ~t ctx in
  match result with
  | [(tapdev,state,Some _ )] -> true
  | _ -> false

let stats ctx t =
  if ctx.dummy then Dummy.stats ctx t else begin
    Stats.t_of_rpc (Jsonrpc.of_string (invoke_tap_ctl ctx "stats" (args t)))
  end

(* We need to be able to check that a given device's major number corresponds to the right driver *)
let read_proc_devices () : (int * string) list =
  let parse_line x = match List.filter (fun x -> x <> "") (String.split ' ' x) with
    | [x; y] -> (try Some (int_of_string x, y) with _ -> None)
    | _ -> None in
  List.concat (List.map Opt.to_list ( Unixext.file_lines_fold (fun acc x -> parse_line x :: acc) [] "/proc/devices") )

let driver_of_major major = List.assoc major (read_proc_devices ())

exception Not_blktap
exception Not_a_device

let of_device ctx path =
  let stat = Unix.stat path in
  if stat.Unix.st_kind <> Unix.S_BLK then raise Not_a_device;
  let major = stat.Unix.st_rdev / 256 in
  let minor = stat.Unix.st_rdev mod 256 in
  if driver_of_major major <> "tapdev" then raise Not_blktap;
  match List.filter (fun (tapdev, _, _) -> tapdev.minor = minor) (list ctx) with
  | [ t ] -> t
  | _ -> raise Not_found
