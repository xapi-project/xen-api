module Listext = Xapi_stdext_std.Listext.List

type bus_type = Xen | Scsi | Floppy | Ide [@@deriving rpcty]

type t = bus_type * int * int [@@deriving rpcty]

let bus_type_to_string = function
  | Xen ->
      "Xen"
  | Scsi ->
      "Scsi"
  | Floppy ->
      "Floppy"
  | Ide ->
      "Ide"

let to_debug_string (bus, disk, partition) =
  Printf.sprintf "%s(%d, %d)" (bus_type_to_string bus) disk partition

let ( let* ) = Option.bind

(* If this is true then we will use the deprecated (linux-specific) IDE
   encodings for disks > 3 *)
let use_deprecated_ide_encoding = true

let max_of = function
  | Xen ->
      ((1 lsl 20) - 1, 15)
  | Scsi ->
      (15, 15)
  | Floppy ->
      (2, 0)
  | Ide ->
      if use_deprecated_ide_encoding then (19, 63) else (3, 63)

let make bus ~disk ~partition =
  let in_range ~min ~max n = min <= n && n <= max in
  let all_in_range (disk_max, partition_max) ~disk ~partition =
    in_range ~min:0 ~max:disk_max disk
    && in_range ~min:0 ~max:partition_max partition
  in
  if all_in_range (max_of bus) ~disk ~partition then
    Some (bus, disk, partition)
  else
    None

let ( || ) = ( lor )

let standard_ide_table = [3; 22]

let deprecated_ide_table = standard_ide_table @ [33; 34; 56; 57; 88; 89; 90; 91]

let to_xenstore_int = function
  | Xen, disk, partition when disk < 16 ->
      (202 lsl 8) || (disk lsl 4) || partition
  | Xen, disk, partition ->
      (1 lsl 28) || (disk lsl 8) || partition
  | Scsi, disk, partition ->
      (8 lsl 8) || (disk lsl 4) || partition
  | Floppy, disk, partition ->
      (203 lsl 8) || (disk lsl 4) || partition
  | Ide, disk, partition ->
      let m = List.nth deprecated_ide_table (disk / 2) in
      let n = disk - (disk / 2 * 2) in
      (* NB integers behave differently to reals *)
      (m lsl 8) || (n lsl 6) || partition

let of_xenstore_int x =
  if x land (1 lsl 28) <> 0 then
    (Xen, (x lsr 8) land ((1 lsl 20) - 1), x land ((1 lsl 8) - 1))
  else
    match x lsr 8 with
    | 202 ->
        (Xen, (x lsr 4) land ((1 lsl 4) - 1), x land ((1 lsl 4) - 1))
    | 8 ->
        (Scsi, (x lsr 4) land ((1 lsl 4) - 1), x land ((1 lsl 4) - 1))
    | 203 ->
        (Floppy, (x lsr 4) land ((1 lsl 4) - 1), x land ((1 lsl 4) - 1))
    | n ->
        let idx =
          match Listext.find_index (Int.equal n) deprecated_ide_table with
          | Some idx ->
              idx
          | None ->
              failwith (Printf.sprintf "Unknown device number: %d" x)
        in
        let disk = ((x lsr 6) land ((1 lsl 2) - 1)) + (idx * 2) in
        let partition = x land ((1 lsl 6) - 1) in
        (Ide, disk, partition)

let to_xenstore_key x = to_xenstore_int x

let of_xenstore_key x = of_xenstore_int x

(* NB the device encoding is base 26 starting from 1 rather than 0 eg 0 -> a 25
   -> z 26 -> aa *)

(** Return an integer encoded as a linux device suffix *)
let rec string_of_int26 x =
  let high, low = ((x / 26) - 1, (x mod 26) + 1) in
  let high' = if high = -1 then "" else string_of_int26 high in
  let low' = String.make 1 (char_of_int (low + int_of_char 'a' - 1)) in
  high' ^ low'

let to_linux_prefix = function
  | Xen ->
      "xvd"
  | Scsi ->
      "sd"
  | Floppy ->
      "fd"
  | Ide ->
      "xvd"

let to_linux_device (bus, disk, part) =
  let p x = if x = 0 then "" else string_of_int x in
  let bus = to_linux_prefix bus in
  Printf.sprintf "%s%s%s" bus (string_of_int26 disk) (p part)

let of_linux_device x =
  let open Astring in
  let b26_to_int x =
    (* Convert a linux device string back into an integer *)
    (* Assumes all characters are in range *)
    let b26 =
      String.Sub.to_string x
      |> Stdlib.String.to_seq
      |> Seq.map (fun c -> int_of_char c - int_of_char 'a' + 1)
      |> Seq.fold_left (fun acc x -> (acc * 26) + x) 0
    in
    b26 - 1
  in

  let parse_int x =
    match String.Sub.span ~min:1 ~sat:Char.Ascii.is_digit x with
    | i, s ->
        Option.map (fun i -> (i, s)) (String.Sub.to_int i)
  in
  let parse_b26 x =
    match String.Sub.span ~min:1 ~sat:Char.Ascii.is_lower x with
    | b, s ->
        (b26_to_int b, s)
  in
  (* Parse a string "abc123" into x, y where x is "abc" interpreted as base-26
     and y is 123 *)
  let parse_b26_int x =
    let pre, x = parse_b26 x in
    if String.Sub.is_empty x then
      Some (pre, 0)
    else
      let* post, x = parse_int x in
      if not (String.Sub.is_empty x) then
        None
      else
        Some (pre, post)
  in
  (* Parse a string "123p456" into x, y where x = 123 and y = 456 *)
  let parse_int_p_int x =
    let parse_p x =
      match String.Sub.head x with
      | Some 'p' ->
          Some (String.Sub.tail x)
      | Some _ | None ->
          None
    in
    let* pre, x = parse_int x in
    if String.Sub.is_empty x then
      Some (pre, 0)
    else
      let* x = parse_p x in
      let* post, x = parse_int x in
      if not (String.Sub.is_empty x) then
        None
      else
        Some (pre, post)
  in
  if String.is_prefix ~affix:"xvd" x then
    let rest = String.sub_with_range ~first:3 x in
    let* disk, partition = parse_b26_int rest in
    Some (Xen, disk, partition)
  else if String.is_prefix ~affix:"sd" x then
    let rest = String.sub_with_range ~first:2 x in
    let* disk, partition = parse_b26_int rest in
    Some (Scsi, disk, partition)
  else if String.is_prefix ~affix:"fd" x then
    let rest = String.sub_with_range ~first:2 x in
    let* disk, partition = parse_b26_int rest in
    Some (Floppy, disk, partition)
  else if String.is_prefix ~affix:"hd" x then
    let rest = String.sub_with_range ~first:2 x in
    let* disk, partition = parse_b26_int rest in
    Some (Ide, disk, partition)
  else if String.is_prefix ~affix:"d" x then
    let rest = String.sub_with_range ~first:1 x in
    let* disk, partition = parse_int_p_int rest in
    Some (Xen, disk, partition)
  else
    None

let upgrade_linux_device x =
  if Astring.String.is_prefix ~affix:"hd" x then
    let rest = Astring.String.with_range ~first:2 x in
    "xvd" ^ rest
  else
    x

let disk (_, disk, _) = disk

let bus (bus, _, _) = bus

let of_disk_number hvm n =
  let bus = if hvm && n < 4 then Ide else Xen in
  make bus ~disk:n ~partition:0

let of_string ~hvm name =
  let maybe_disk =
    let* n = int_of_string_opt name in
    of_disk_number hvm n
  in
  match maybe_disk with None -> of_linux_device name | dev -> dev
