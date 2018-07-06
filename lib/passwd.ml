open Ctypes
open Foreign

type file_descr = unit ptr
let file_descr : file_descr typ = ptr void

let fopen =
  foreign ~check_errno:true "fopen" (string @-> string @-> returning file_descr)
let fclose' = foreign ~check_errno:true "fclose" (file_descr @-> returning int)
let fclose fd = fclose' fd |> ignore

type t = {
  name   : string;
  passwd : string;
  (* According to bits/typesizes.h uid_t and gid_t are uint32 *)
  uid    : int; (* uid    : uid_t; *)
  gid    : int; (* gid    : gid_t; *)
  gecos  : string;
  dir    : string;
  shell  : string;
}

type db = t list

type passwd_t

let passwd_t : passwd_t structure typ = structure "passwd"

let pw_name   = field passwd_t "pw_name" (ptr char)
let pw_passwd = field passwd_t "pw_passwd" (ptr char)
let pw_uid    = field passwd_t "pw_uid" uint32_t
let pw_gid    = field passwd_t "pw_gid" uint32_t
let pw_gecos  = field passwd_t "pw_gecos" (ptr char)
let pw_dir    = field passwd_t "pw_dir" (ptr char)
let pw_shell  = field passwd_t "pw_shell" (ptr char)

let () = seal passwd_t

let ptr_char_to_string p = coerce (ptr char) string p

let string_to_char_array s =
  let len = String.length s in
  let buf = CArray.make char ~initial:'\x00' (len+1) in
  String.iteri (fun idx c -> CArray.set buf idx c) s;
  buf

let from_passwd_t pw = {
  name   = getf pw pw_name |> ptr_char_to_string;
  passwd = getf pw pw_passwd |> ptr_char_to_string;
  uid    = getf pw pw_uid |> Unsigned.UInt32.to_int;
  gid    = getf pw pw_gid |> Unsigned.UInt32.to_int;
  gecos  = getf pw pw_gecos |> ptr_char_to_string;
  dir    = getf pw pw_dir |> ptr_char_to_string;
  shell  = getf pw pw_shell |> ptr_char_to_string;
}

let from_passwd_t_opt = function
  | None -> None
  | Some pw -> Some (from_passwd_t !@pw)

module Mem : sig
  type mem
  val to_mem : t -> mem
  val passwd_addr_of_mem : mem -> (passwd_t, [`Struct]) Ctypes.structured Ctypes.ptr
end = struct
  type mem = passwd_t structure * char carray * char carray * char carray * char carray * char carray

  let to_mem pw =
    let name = string_to_char_array pw.name in
    let passwd = string_to_char_array pw.passwd in
    let gecos = string_to_char_array pw.gecos in
    let dir = string_to_char_array pw.dir in
    let shell = string_to_char_array pw.shell in
    let pw_t : passwd_t structure = make passwd_t in
    setf pw_t pw_name (CArray.start name);
    setf pw_t pw_passwd (CArray.start passwd);
    setf pw_t pw_uid (Unsigned.UInt32.of_int pw.uid);
    setf pw_t pw_gid (Unsigned.UInt32.of_int pw.gid);
    setf pw_t pw_gecos (CArray.start gecos);
    setf pw_t pw_dir (CArray.start dir);
    setf pw_t pw_shell (CArray.start shell);
    (pw_t, name, passwd, gecos, dir, shell)

  let passwd_addr_of_mem (sp_t, _, _, _, _, _) = addr sp_t
end

let passwd_file = "/etc/passwd"

let getpwnam' =
  foreign ~check_errno:true "getpwnam" (string @-> returning (ptr_opt passwd_t))
let getpwnam name = getpwnam' name |> from_passwd_t_opt

let getpwuid' =
  foreign ~check_errno:true "getpwuid" (int @-> returning (ptr_opt passwd_t))
let getpwuid uid = getpwuid' uid |> from_passwd_t_opt

let getpwent' =
  foreign ~check_errno:true "getpwent" (void @-> returning (ptr_opt passwd_t))
let getpwent () = getpwent' () |> from_passwd_t_opt

let setpwent = foreign ~check_errno:true "setpwent" (void @-> returning void)
let endpwent = foreign ~check_errno:true "endpwent" (void @-> returning void)

let putpwent' =
  foreign ~check_errno:true "putpwent" (ptr passwd_t @-> file_descr @-> returning int)
let putpwent fd pw =
  let mem = Mem.to_mem pw in
  putpwent' (Mem.passwd_addr_of_mem mem) fd |> ignore

let get_db () =
  let rec loop acc =
    match getpwent () with
    | None -> endpwent () ; acc
    | Some pw -> loop (pw :: acc)
  in
  setpwent () ;
  loop [] |> List.rev

let update_db db ent =
  let rec loop acc = function
    | [] -> List.rev acc
    | e :: es when e.name = ent.name -> loop (ent::acc) es
    | e :: es -> loop (e::acc) es
  in loop [] db

let write_db ?(file=passwd_file) db =
  let fd = fopen file "r+" in
  List.iter (putpwent fd) db;
  fclose fd

let to_string p =
  let str i =
    if i >= 0
    then string_of_int i
    else "" in
  Printf.sprintf "%s:%s:%s:%s:%s:%s:%s"
    p.name
    p.passwd
    (str p.uid)
    (str p.gid)
    p.gecos
    p.dir
    p.shell

let db_to_string db =
  db
  |> List.map to_string
  |> String.concat "\n"

(* Local Variables: *)
(* indent-tabs-mode: nil *)
(* End: *)
