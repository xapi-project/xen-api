open Ctypes
open Foreign

type t = {
  name     : string;
  passwd   : string;
  last_chg : int64;
  min      : int64;
  max      : int64;
  warn     : int64;
  inact    : int64;
  expire   : int64;
  flag     : int;
}

type db = t list

type shadow_t

let shadow_t : shadow_t structure typ = structure "passwd"

let sp_name     = field shadow_t "sp_name" (ptr char)
let sp_passwd   = field shadow_t "sp_passwd" (ptr char)
let sp_last_chg  = field shadow_t "sp_lastchg" long
let sp_min      = field shadow_t "sp_min" long
let sp_max      = field shadow_t "sp_max" long
let sp_warn     = field shadow_t "sp_warn" long
let sp_inact    = field shadow_t "sp_inact" long
let sp_expire   = field shadow_t "sp_expire" long
let sp_flag     = field shadow_t "sp_flag" ulong

let () = seal shadow_t

let ptr_char_to_string = coerce (ptr char) string

let string_to_char_array s =
  let len = String.length s in
  let buf = CArray.make char ~initial:'\x00' (len+1) in
  String.iteri (fun idx c -> CArray.set buf idx c) s;
  buf

let from_shadow_t sp = {
  name     = getf sp sp_name |> ptr_char_to_string;
  passwd   = getf sp sp_passwd |> ptr_char_to_string;
  last_chg = getf sp sp_last_chg |> Signed.Long.to_int64;
  min      = getf sp sp_min |> Signed.Long.to_int64;
  max      = getf sp sp_max |> Signed.Long.to_int64;
  warn     = getf sp sp_warn |> Signed.Long.to_int64;
  inact    = getf sp sp_inact |> Signed.Long.to_int64;
  expire   = getf sp sp_expire |> Signed.Long.to_int64;
  flag     = getf sp sp_flag |> Unsigned.ULong.to_int;
}

let from_shadow_t_opt = function
  | None -> None
  | Some sp -> Some (from_shadow_t !@sp)

(* Wrap up all the allocating functions into this module that
   returns an opaque type. This tries to make sure that the
   lifetime of all the allocated memory is the same. The one
   dangerous thing is 'shadow_addr_of_mem' - but it's a bit
   more obvious that you've got to keep the 'mem' value alive
   than the individual fields within it too. Note that we
   actually hide `shadow_addr_of_mem` from outside this module
   via the mli file. Paranoia! *)
module Mem : sig
  type mem
  val to_mem : t -> mem
  val from_mem : mem -> t
  val shadow_addr_of_mem : mem -> (shadow_t, [ `Struct ]) Ctypes.structured Ctypes.ptr
end = struct
  type mem = shadow_t structure * char carray * char carray

  let to_mem sp =
    let name = string_to_char_array sp.name in
    let passwd = string_to_char_array sp.passwd in
    let sp_t : shadow_t structure = make shadow_t in
    setf sp_t sp_name (CArray.start name);
    setf sp_t sp_passwd (CArray.start passwd);
    setf sp_t sp_last_chg (Signed.Long.of_int64 sp.last_chg);
    setf sp_t sp_min (Signed.Long.of_int64 sp.min);
    setf sp_t sp_max (Signed.Long.of_int64 sp.max);
    setf sp_t sp_warn (Signed.Long.of_int64 sp.warn);
    setf sp_t sp_inact (Signed.Long.of_int64 sp.inact);
    setf sp_t sp_expire (Signed.Long.of_int64 sp.expire);
    setf sp_t sp_flag (Unsigned.ULong.of_int sp.flag);
    (sp_t,name,passwd)

  let from_mem (sp_t, _name, _passwd) = from_shadow_t sp_t

  let shadow_addr_of_mem (sp_t, _, _) = addr sp_t
end

let shadow_file = "/etc/shadow"

let getspnam' =
  foreign ~check_errno:true "getspnam" (string @-> returning (ptr_opt shadow_t))
let getspnam name = getspnam' name |> from_shadow_t_opt

let getspent' =
  foreign ~check_errno:true "getspent" (void @-> returning (ptr_opt shadow_t))
let getspent () = getspent' () |> from_shadow_t_opt

let setspent = foreign ~check_errno:true "setspent" (void @-> returning void)
let endspent = foreign ~check_errno:true "endspent" (void @-> returning void)

let putspent' =
  foreign ~check_errno:true
    "putspent" (ptr shadow_t @-> Passwd.file_descr @-> returning int)
let putspent fd sp =
  let mem = Mem.to_mem sp in
  putspent' (Mem.shadow_addr_of_mem mem) fd |> ignore

let lckpwdf' = foreign "lckpwdf" (void @-> returning int)
let lckpwdf () = lckpwdf' () = 0

let ulckpwdf' = foreign "ulckpwdf" (void @-> returning int)
let ulckpwdf () = ulckpwdf' () = 0

let shadow_enabled () =
  try Unix.access shadow_file [Unix.F_OK]; true with _ -> false

let get_db () =
  let rec loop acc =
    match getspent () with
    | None -> endspent () ; acc
    | Some sp -> loop (sp :: acc)
  in
  setspent () ;
  loop [] |> List.rev

let update_db db pw =
  let rec loop acc = function
    | [] -> List.rev acc
    | e :: es when e.name = pw.name -> loop (pw::acc) es
    | e :: es -> loop (e::acc) es
  in loop [] db

let write_db ?(file=shadow_file) db =
  let fd = Passwd.fopen file "r+" in
  List.iter (putspent fd) db;
  Passwd.fclose fd

let to_string p =
  let str i =
    if (Int64.compare i 0L) >= 0
    then Int64.to_string i
    else "" in
  Printf.sprintf "%s:%s:%s:%s:%s:%s:%s:%s:%d"
    p.name
    p.passwd
    (str p.last_chg)
    (str p.min)
    (str p.max)
    (str p.warn)
    (str p.inact)
    (str p.expire)
    p.flag

let db_to_string db =
  db
  |> List.map to_string
  |> String.concat "\n"

let with_lock f =
  if lckpwdf ()
  then begin
    let ret = f () in
    ignore (ulckpwdf ());
    ret
  end
  else
    raise Unix.(Unix_error (EAGAIN, "with_lock", "Couldn't acquire shadow lock"))

(* Local Variables: *)
(* indent-tabs-mode: nil *)
(* End: *)
