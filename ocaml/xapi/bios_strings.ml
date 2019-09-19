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
  let l = String.split_on_char '\n' str in
  let l = List.filter (fun s -> not (String.startswith "#" s)) l in
  let str = String.concat "\n" l in
  String.fold_left (fun s c -> if c >= ' ' && c <= '~' then s ^ (String.of_char c) else s) "" str

(* A single record from the output of dmidecode,
 * without its type, handle nor size.
 * Arrays are represented by a string with values separated by eol *)
type record = { name: string; values: (string * string) list; }

(* Parser module for the output of command dmidecode -q *)
module P = struct
  open Angstrom
  let is_eol =   function | '\n' -> true | _ -> false
  let is_tab =   function | '\t' -> true | _ -> false
  let is_space = function | ' '  -> true | _ -> false
  let is_sep =   function | ':'  -> true | _ -> false

  let space = skip is_space
  let sep = skip is_sep
  let tab = skip is_tab

  let item = take_till is_eol <* end_of_line
  let key = tab *> take_till is_sep <* sep
  let array_count = space *> item
    >>= fun str -> match int_of_string_opt str with
    | Some n -> return (Some n)
    | None -> fail "Couldn't parse number"
  let value_line = tab *> tab *> item

  (* Inline value example:
   *	String 1: Dell System
   *)
  let inline_value =
    lift2 (fun k v -> k, v)
    key (* k *)
    (space *> item) (* v *)

  (* Multiple-line value examples:
   *	Features:
   *		Board is a hosting board
   *		Board is replaceable
   *
   *	Items: 1
   *		0x0000 (OEM-specific)
   *)
  let multi_value =
    lift3 (fun k _ lines -> k, String.concat "\n" lines)
    key (* k *)
    ((end_of_line >>| fun _ -> None) <|> array_count) (* number of elements, ignored *)
    (many1 value_line) (* lines *)

  let record =
    lift2 (fun name values -> {name; values})
    (item) (* name *)
    (many (multi_value <|> inline_value)) <* end_of_line (* values *)

  let records = many record
end

let get_output_of_type e_type =
  try
    let output, _ = Forkhelpers.execute_command_get_output dmidecode_prog [dmidecode_prog; "-qt"; e_type] in
    output
  with _ ->
    ""

(* obtain the BIOS string with the given name from dmidecode *)
let get_bios_string name =
  try
    let str, _ = Forkhelpers.execute_command_get_output dmidecode_prog [dmidecode_prog; "-s"; name] in
    let str = String.trim (remove_invisible str) in
    if str = "" || str = "Not Specified" then ""
    else str
  with _ -> ""

let get_strings name keys key_values =
  let convert (key, value) =
    let key = key
      |> String.lowercase_ascii
      |> String.map (function | ' ' -> '-' | c -> c)
      |> Printf.sprintf "%s-%s" name in
    let value = match value with
      | "Not Specified" -> ""
      | v -> remove_invisible v |> String.trim in
    key, value in
  let formatted_key_values = List.map convert key_values in
  let get_value_for key =
    let value = match List.assoc_opt key formatted_key_values with
      | Some v -> v
      | None -> ""
    in
    key, value
  in
  List.map get_value_for keys

let get_dmidecode_strings e_type name =
  let output = get_output_of_type e_type in
  match Angstrom.parse_string P.records output with
  | Ok (r :: _) -> r.values
  | Ok [] -> warn "No %s records found" name; []
  | Error msg -> warn "Command dmidecode failed for %s: %s" name msg; []

(* Obtain the Type 11 OEM strings from dmidecode, and prepend with the standard ones. *)
let get_oem_strings decode =
  let standard = Xapi_globs.standard_type11_strings in
  let start_index = List.length standard + 1 in
  let values = decode "11" "oem" in

  let convert i (_, value) =
    Printf.sprintf "oem-%d" (i + start_index), remove_invisible value |> String.trim in
  standard @ List.mapi convert values

(* Get the HP-specific ROMBIOS OEM string:
 * 6 bytes from the memory starting at 0xfffea *)
let get_hp_rombios () =
  let hp_rombios = Bytes.make 6 ' ' in
  begin try
      let mem = Unix.openfile "/dev/mem" [Unix.O_RDONLY] 0 in
      Stdext.Pervasiveext.finally (fun () ->
          ignore (Unix.lseek mem 0xfffea Unix.SEEK_SET);
          ignore (Unix.read mem hp_rombios 0 6))
        (fun () -> Unix.close mem)
    with _ -> ()
  end;
  let hp_rombios = Bytes.unsafe_to_string hp_rombios in
  if String.trim (remove_invisible hp_rombios) = "COMPAQ" then "COMPAQ" else ""

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
  let oem_strings = get_oem_strings get_dmidecode_strings in
  (* HP-specific ROMBIOS OEM string *)
  let hp_rombios = ["hp-rombios", get_hp_rombios ()] in
  named_strings @ oem_strings @ hp_rombios
