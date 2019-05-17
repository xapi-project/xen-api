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
module D = Debug.Make(struct let name="bios_strings" end)
open D
open Stdext.Xstringext

let dmidecode_prog = "/usr/sbin/dmidecode"

let remove_invisible str =
  let l = String.split '\n' str in
  let l = List.filter (fun s -> not (String.startswith "#" s)) l in
  let str = String.concat "" l in
  String.fold_left (fun s c -> if c >= ' ' && c <= '~' then s ^ (String.of_char c) else s) "" str

let trim str =
  let l = String.length str in
  let rec check_left i =
    if i < l && String.isspace str.[i] then
      check_left (i+1)
    else
      i
  in
  let rec check_right i =
    if i > 0 && String.isspace str.[i] then
      check_right (i-1)
    else
      i+1
  in
  let a = check_left 0 in
  let b = (check_right (l-1)) - a in
  try	String.sub str a b with Invalid_argument _  -> ""

(* obtain the BIOS string with the given name from dmidecode *)
let get_bios_string name =
  try
    let str, _ = Forkhelpers.execute_command_get_output dmidecode_prog [dmidecode_prog; "-s"; name] in
    let str = trim (remove_invisible str) in
    if str = "" || str = "Not Specified" then ""
    else str
  with _ -> ""

(* Obtain the Type 11 OEM strings from dmidecode, and prepend with the standard ones. *)
let get_oem_strings () =
  let standard = Xapi_globs.standard_type11_strings in
  try
    let result, _ = Forkhelpers.execute_command_get_output dmidecode_prog [dmidecode_prog; "-t11"; "-q"] in
    let n = List.length standard in
    let rec loop index a =
      try
        let b = String.index_from result a ':' in
        let c = String.index_from result b '\n' in
        let str = "oem-" ^ (string_of_int index) in
        let value = trim (remove_invisible (String.sub result (b+2) (c-b-2))) in
        if value <> "" then
          (str, value) :: loop (index+1) c
        else
          loop index c
      with _ -> []
    in
    standard @ (loop (n+1) 0)
  with _ -> standard

(* Get the HP-specific ROMBIOS OEM string:
 * 6 bytes from the memory starting at 0xfffea *)
let get_hp_rombios () =
  let hp_rombios = String.make 6 ' ' in
  begin try
      let mem = Unix.openfile "/dev/mem" [Unix.O_RDONLY] 0 in
      Stdext.Pervasiveext.finally (fun () ->
          ignore (Unix.lseek mem 0xfffea Unix.SEEK_SET);
          ignore (Unix.read mem hp_rombios 0 6))
        (fun () -> Unix.close mem)
    with _ -> ()
  end;
  if trim (remove_invisible hp_rombios) = "COMPAQ" then "COMPAQ" else ""

(* Get host bios strings *)
let get_host_bios_strings ~__context =
  info "Getting host BIOS strings.";
  (* named BIOS strings *)
  let dmidecode_strings = ["bios-vendor"; "bios-version"; "system-manufacturer";
                           "system-product-name"; "system-version"; "system-serial-number";
                           "baseboard-manufacturer"; "baseboard-product-name";
                           "baseboard-version"; "baseboard-serial-number";
                          ] in
  let named_strings = List.map (fun str -> str, (get_bios_string str)) dmidecode_strings in
  (* type 11 OEM strings *)
  let oem_strings = get_oem_strings () in
  (* HP-specific ROMBIOS OEM string *)
  let hp_rombios = ["hp-rombios", get_hp_rombios ()] in
  named_strings @ oem_strings @ hp_rombios
