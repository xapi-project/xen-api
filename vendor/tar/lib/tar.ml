(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


(** Process and create tar file headers *)
module Header = struct
  (** Map of field name -> (start offset, length) taken from wikipedia:
      http://en.wikipedia.org/w/index.php?title=Tar_%28file_format%29&oldid=83554041 *)

  (** For debugging: pretty-print a string as hex *)
  let to_hex (x: string) : string =
    let result = Bytes.make (String.length x * 3) ' ' in
    for i = 0 to String.length x - 1 do
      let byte = Printf.sprintf "%02x" (int_of_char x.[i]) in
      Bytes.blit_string byte 0 result (i * 3) 2
    done;
    Bytes.to_string result

  let trim regexp x = match Re_str.split regexp x with
    | [] -> ""
    | x :: _ -> x
  let trim_numerical = trim (Re_str.regexp "[\000 ]+")
  let trim_string = trim (Re_str.regexp "[\000]+")

  (** Unmarshal an integer field (stored as 0-padded octal) *)
  let unmarshal_int (x: string) : int =
    let tmp = "0o0" ^ (trim_numerical x) in
    try
      int_of_string tmp
    with Failure _ as e ->
      Printf.eprintf "Failed to parse integer [%s] == %s\n" tmp (to_hex tmp);
      raise e

  (** Unmarshal an int64 field (stored as 0-padded octal) *)
  let unmarshal_int64 (x: string) : int64 =
    let tmp = "0o0" ^ (trim_numerical x) in
    Int64.of_string tmp

  (** Unmarshal a string *)
  let unmarshal_string (x: string) : string = trim_string x

  [%%cstruct
      type hdr = {
        file_name:      uint8_t [@len 100];
        file_mode:      uint8_t [@len 8];
        user_id:        uint8_t [@len 8];
        group_id:       uint8_t [@len 8];
        file_size:      uint8_t [@len 12];
        mod_time:       uint8_t [@len 12];
        chksum:         uint8_t [@len 8];
        link_indicator: uint8_t ;
        link_name:      uint8_t [@len 100];
        magic:          uint8_t [@len 6];
        version:        uint8_t [@len 2];
        uname:          uint8_t [@len 32];
        gname:          uint8_t [@len 32];
        devmajor:       uint8_t [@len 8];
        devminor:       uint8_t [@len 8];
        prefix:         uint8_t [@len 155]
      } [@@little_endian]
  ] (* doesn't matter, all are strings *)

  let sizeof_hdr_file_name = 100
  let sizeof_hdr_file_mode = 8
  let sizeof_hdr_user_id   = 8
  let sizeof_hdr_group_id  = 8
  let sizeof_hdr_file_size = 12
  let sizeof_hdr_mod_time  = 12
  let sizeof_hdr_chksum    = 8
  let sizeof_hdr_link_name = 100
  let sizeof_hdr_magic = 6
  let sizeof_hdr_version = 2
  let sizeof_hdr_uname = 32
  let sizeof_hdr_gname = 32
  let sizeof_hdr_devmajor = 8
  let sizeof_hdr_devminor = 8
  let sizeof_hdr_prefix = 155

  type compatibility =
    | OldGNU
    | GNU
    | V7
    | Ustar
    | Posix

  let compatibility_level = ref V7

  let get_level = function
    | None -> !compatibility_level
    | Some level -> level

  module Link = struct
    type t =
      | Normal
      | Hard
      | Symbolic
      | Character
      | Block
      | Directory
      | FIFO
      | GlobalExtendedHeader
      | PerFileExtendedHeader
      | LongLink

    (* Strictly speaking, v7 supports Normal (as \0) and Hard only *)
    let to_int ?level =
      let level = get_level level in function
        | Normal                -> if level = V7 then 0 else 48 (* '0' *)
        | Hard                  -> 49 (* '1' *)
        | Symbolic              -> 50 (* '2' *)
        | Character             -> 51 (* '3' *)
        | Block                 -> 52 (* '4' *)
        | Directory             -> 53 (* '5' *)
        | FIFO                  -> 54 (* '6' *)
        | GlobalExtendedHeader  -> 103 (* 'g' *)
        | PerFileExtendedHeader -> 120 (* 'x' *)
        | LongLink              -> 76  (* 'L' *)

    let of_int ?level =
      let level = get_level level in function
        | 49 (* '1' *) -> Hard
        | 50 (* '2' *) -> Symbolic
        | 103 (* 'g' *) -> GlobalExtendedHeader
        | 120 (* 'x' *) -> PerFileExtendedHeader
        | 76 (* 'L' *) -> LongLink
        (* All other types returned as Normal in V7 for compatibility with older versions of ocaml-tar *)
        | _ when level = V7 -> Normal (* if value is malformed, treat as a normal file *)
        | 51 (* '3' *) -> Character
        | 52 (* '4' *) -> Block
        | 53 (* '5' *) -> Directory
        | 54 (* '6' *) -> FIFO
        | _ -> Normal (* if value is malformed, treat as a normal file *)

    let to_string = function
      | Normal -> "Normal"
      | Hard -> "Hard"
      | Symbolic -> "Symbolic"
      | Character -> "Character"
      | Block -> "Block"
      | Directory -> "Directory"
      | FIFO -> "FIFO"
      | GlobalExtendedHeader -> "GlobalExtendedHeader"
      | PerFileExtendedHeader -> "PerFileExtendedHeader"
      | LongLink -> "LongLink"
  end

  module Extended = struct
    type t = {
      access_time: int64 option;
      charset: string option;
      comment: string option;
      group_id: int option;
      gname: string option;
      header_charset: string option;
      link_path: string option;
      mod_time: int64 option;
      path: string option;
      file_size: int64 option;
      user_id: int option;
      uname: string option;
    }
    (** Represents a "Pax" extended header *)

    let to_detailed_string (x: t) =
      let opt f = function None -> "None" | Some x -> f x in
      let int64 = Int64.to_string and string x = x and int = string_of_int in
      let table = [ "access_time",    opt int64 x.access_time;
                    "charset",        opt string x.charset;
                    "comment",        opt string x.comment;
                    "group_id",       opt int x.group_id;
                    "gname",          opt string x.gname;
                    "header_charset", opt string x.header_charset;
                    "link_path",      opt string x.link_path;
                    "mod_time",       opt int64 x.mod_time;
                    "path",           opt string x.path;
                    "file_size",      opt int64 x.file_size;
                    "user_id",        opt int x.user_id;
                    "uname",          opt string x.uname ] in
      "{\n" ^ (String.concat "\n\t" (List.map (fun (k, v) -> k ^ ": " ^ v) table)) ^ "}"

    let make ?access_time ?charset ?comment ?group_id ?gname ?header_charset
      ?link_path ?mod_time ?path ?file_size ?user_id ?uname () =
      { access_time; charset; comment; group_id; gname; header_charset;
        link_path; mod_time; path; file_size; user_id; uname }

    let marshal t =
      let pairs =
        (match t.access_time    with None -> [] | Some x -> [ "atime", Int64.to_string x ])
      @ (match t.charset        with None -> [] | Some x -> [ "charset", x ])
      @ (match t.comment        with None -> [] | Some x -> [ "comment", x ])
      @ (match t.group_id       with None -> [] | Some x -> [ "gid", string_of_int x ])
      @ (match t.gname          with None -> [] | Some x -> [ "group_name", x ])
      @ (match t.header_charset with None -> [] | Some x -> [ "hdrcharset", x ])
      @ (match t.link_path      with None -> [] | Some x -> [ "linkpath", x ])
      @ (match t.mod_time       with None -> [] | Some x -> [ "mtime", Int64.to_string x ])
      @ (match t.path           with None -> [] | Some x -> [ "path", x ])
      @ (match t.file_size      with None -> [] | Some x -> [ "size", Int64.to_string x ])
      @ (match t.user_id        with None -> [] | Some x -> [ "uid", string_of_int x ])
      @ (match t.uname          with None -> [] | Some x -> [ "uname", x ]) in
      let txt = String.concat "" (List.map (fun (k, v) ->
        let length = 8 + 1 + (String.length k) + 1 + (String.length v) + 1 in
        Printf.sprintf "%08d %s=%s\n" length k v
      ) pairs) in
      let buffer = Cstruct.create (String.length txt) in
      Cstruct.blit_from_string txt 0 buffer 0 (String.length txt);
      buffer

    let unmarshal (c: Cstruct.t) : t =
      (* "%d %s=%s\n", <length>, <keyword>, <value> with constraints that
         - the <keyword> cannot contain an equals sign
         - the <length> is the number of octets of the record, including \n
        *)
      let find buffer char =
        let rec loop i =
          if i = Cstruct.len buffer
          then None
          else if Cstruct.get_char buffer i = char
          then Some i
          else loop (i + 1) in
        loop 0 in
      let rec loop remaining =
        if Cstruct.len remaining = 0
        then []
        else begin
          (* Find the space, then decode the length *)
          match find remaining ' ' with
          | None -> failwith "Failed to decode pax extended header record"
          | Some i ->
            let length = int_of_string @@ Cstruct.to_string @@ Cstruct.sub remaining 0 i in
            let record = Cstruct.sub remaining 0 length in
            let remaining = Cstruct.shift remaining length in
            begin match find record '=' with
            | None -> failwith "Failed to decode pax extended header record"
            | Some j ->
              let keyword = Cstruct.to_string @@ Cstruct.sub record (i + 1) (j - i - 1) in
              let v = Cstruct.to_string @@ Cstruct.sub record (j + 1) (Cstruct.len record - j - 2) in
              (keyword, v) :: (loop remaining)
            end
        end in
      let pairs = loop c in
      let option name f =
        if List.mem_assoc name pairs
        then Some (f (List.assoc name pairs))
        else None in
      (* integers are stored as decimal, not octal here *)
      let access_time    = option "atime" Int64.of_string in
      let charset        = option "charset" unmarshal_string in
      let comment        = option "comment" unmarshal_string in
      let group_id       = option "gid" int_of_string in
      let gname          = option "group_name" unmarshal_string in
      let header_charset = option "hdrcharset" unmarshal_string in
      let link_path      = option "linkpath" unmarshal_string in
      let mod_time       = option "mtime" Int64.of_string in
      let path           = option "path" unmarshal_string in
      let file_size      = option "size" Int64.of_string in
      let user_id        = option "uid" int_of_string in
      let uname          = option "uname" unmarshal_string in
      { access_time; charset; comment; group_id; gname;
        header_charset; link_path; mod_time; path; file_size;
        user_id; uname }

  end

  (** Represents a standard (non-USTAR) archive (note checksum not stored) *)
  type t = { file_name: string;
             file_mode: int;
             user_id: int;
             group_id: int;
             file_size: int64;
             mod_time: int64;
             link_indicator: Link.t;
             link_name: string;
             uname: string;
             gname: string;
             devmajor: int;
             devminor: int;
             extended: Extended.t option;
           }

  (** Helper function to make a simple header *)
  let make ?(file_mode=0) ?(user_id=0) ?(group_id=0) ?(mod_time=0L) ?(link_indicator=Link.Normal) ?(link_name="") ?(uname="") ?(gname="") ?(devmajor=0) ?(devminor=0) file_name file_size =
    (* If some fields are too big, we must use a pax header *)
    let need_pax_header =
       file_size   > 0o077777777777L
       || user_id  > 0x07777777
       || group_id > 0x07777777 in
    let extended =
      if need_pax_header
      then Some (Extended.make ~file_size ~user_id ~group_id ())
      else None in
    { file_name;
      file_mode;
      user_id;
      group_id;
      file_size;
      mod_time;
      link_indicator;
      link_name;
      uname;
      gname;
      devmajor;
      devminor;
      extended }

  (** Length of a header block *)
  let length = 512

  (** A blank header block (two of these in series mark the end of the tar) *)
  let zero_block =
    let buf = Cstruct.create length in
    for i = 0 to Cstruct.len buf - 1 do
      Cstruct.set_uint8 buf i 0
    done;
    buf

  (** [allzeroes buf] is true if [buf] contains only zero bytes *)
  let allzeroes buf =
    let rec loop i =
      (i >= Cstruct.len buf) || (Cstruct.get_uint8 buf i = 0 && (loop (i + 1))) in
    loop 0

  (** Return a string containing 'x' padded out to 'n' bytes by adding 'c' to the LHS *)
  let pad_left (x: string) (n: int) (c: char) =
    if String.length x >= n then x
    else let buffer = Bytes.make n c in
      Bytes.blit_string x 0 buffer (n - (String.length x)) (String.length x);
      Bytes.to_string buffer

  (** Return a string containing 'x' padded out to 'n' bytes by adding 'c' to the RHS *)
  let pad_right (x: string) (n: int) (c: char) =
    if String.length x >= n then x
    else let buffer = Bytes.make n c in
      Bytes.blit_string x 0 buffer 0 (String.length x);
      Bytes.to_string buffer

  (** Pretty-print the header record *)
  let to_detailed_string (x: t) =
    let table = [ "file_name",      x.file_name;
                  "file_mode",      string_of_int x.file_mode;
                  "user_id",        string_of_int x.user_id;
                  "group_id",       string_of_int x.group_id;
                  "file_size",      Int64.to_string x.file_size;
                  "mod_time",       Int64.to_string x.mod_time;
                  "link_indicator", Link.to_string x.link_indicator;
                  "link_name",      x.link_name ] in
    "{\n" ^ (String.concat "\n\t" (List.map (fun (k, v) -> k ^ ": " ^ v) table)) ^ "}"

  (** Marshal an integer field of size 'n' *)
  let marshal_int (x: int) (n: int) =
    let octal = Printf.sprintf "%o" x in
    let result = pad_left octal (n-1) '0' in
    result ^ "\000" (* space or NULL allowed *)

  (** Marshal an int64 field of size 'n' *)
  let marshal_int64 (x: int64) (n: int) =
    let octal = Printf.sprintf "%Lo" x in
    let result = pad_left octal (n-1) '0' in
    result ^ "\000" (* space or NULL allowed *)

  (** Marshal an string field of size 'n' *)
  let marshal_string (x: string) (n: int) = pad_right x n '\000'

  (** Thrown when unmarshalling a header if the checksums don't match *)
  exception Checksum_mismatch

  (** From an already-marshalled block, compute what the checksum should be *)
  let checksum (x: Cstruct.t) : int64 =
    (* Sum of all the byte values of the header with the checksum field taken
       as 8 ' ' (spaces) *)
    let result = ref 0 in
    for i = 0 to Cstruct.len x - 1 do
      result := !result + (Cstruct.get_uint8 x i)
    done;
    (* since we included the checksum, subtract it and add the spaces *)
    let chksum = get_hdr_chksum x in
    for i = 0 to Cstruct.len chksum - 1 do
      result := !result - (Cstruct.get_uint8 chksum i) + (int_of_char ' ')
    done;
    Int64.of_int !result

  (** Unmarshal a header block, returning None if it's all zeroes *)
  let unmarshal ?level ?(extended = Extended.make ()) (c: Cstruct.t) : t option =
    let level = get_level level in
    if allzeroes c then None
    else
      let chksum = unmarshal_int64 (copy_hdr_chksum c) in
      if checksum c <> chksum then raise Checksum_mismatch
      else let ustar =
             let magic = unmarshal_string (copy_hdr_magic c) in
             (* GNU tar and Posix differ in interpretation of the character following ustar. For Posix, it should be '\0' but GNU tar uses ' ' *)
             String.length magic >= 5 && (String.sub magic 0 5 = "ustar") in
        let prefix = if ustar then unmarshal_string (copy_hdr_prefix c) else "" in
        let file_name =
          let file_name = unmarshal_string (copy_hdr_file_name c) in
          if file_name = "" then prefix
          else if prefix = "" then file_name
          else Filename.concat prefix file_name in
        let file_mode = unmarshal_int (copy_hdr_file_mode c) in
        let user_id = match extended.Extended.user_id with
          | None -> unmarshal_int (copy_hdr_user_id c)
          | Some x -> x in
        let group_id = match extended.Extended.group_id with
          | None -> unmarshal_int (copy_hdr_group_id c)
          | Some x -> x in
        let file_size = match extended.Extended.file_size with
          | None -> unmarshal_int64 (copy_hdr_file_size c)
          | Some x -> x in
        let mod_time = match extended.Extended.mod_time with
          | None -> unmarshal_int64  (copy_hdr_mod_time c)
          | Some x -> x in
        let link_indicator = Link.of_int ~level (get_hdr_link_indicator c) in
        let uname = match extended.Extended.uname with
          | None -> if ustar then unmarshal_string (copy_hdr_uname c) else ""
          | Some x -> x in
        let gname = match extended.Extended.gname with
          | None -> if ustar then unmarshal_string (copy_hdr_gname c) else ""
          | Some x -> x in
        let devmajor  = if ustar then unmarshal_int (copy_hdr_devmajor c) else 0 in
        let devminor  = if ustar then unmarshal_int (copy_hdr_devminor c) else 0 in

        let link_name = unmarshal_string (copy_hdr_link_name c) in
        Some (make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator
           ~link_name ~uname ~gname ~devmajor ~devminor file_name file_size)

  (** Marshal a header block, computing and inserting the checksum *)
  let imarshal ~level c link_indicator (x: t) =
    (* The caller (e.g. write_block) is expected to insert the extra ././@LongLink header *)
    if String.length x.file_name > sizeof_hdr_file_name && level <> GNU then
      if level = Ustar then
        if String.length x.file_name > 256 then failwith "file_name too long"
        else let (prefix, file_name) =
               let is_directory = if x.file_name.[String.length x.file_name - 1] = '/' then "/" else "" in
               let rec split prefix file_name =
                 if String.length file_name > sizeof_hdr_file_name then failwith "file_name can't be split"
                 else if String.length prefix > sizeof_hdr_prefix then split (Filename.dirname prefix) (Filename.concat (Filename.basename prefix) file_name ^ is_directory)
                 else (prefix, file_name) in
               split (Filename.dirname x.file_name) (Filename.basename x.file_name ^ is_directory) in
          set_hdr_file_name (marshal_string file_name sizeof_hdr_file_name) 0 c;
          set_hdr_prefix (marshal_string prefix sizeof_hdr_prefix) 0 c
      else failwith "file_name too long"
    else set_hdr_file_name (marshal_string x.file_name sizeof_hdr_file_name) 0 c;
    (* This relies on the fact that the block was initialised to null characters *)
    if level = Ustar || (level = GNU && x.devmajor = 0 && x.devminor = 0) then begin
      if level = Ustar then begin
        set_hdr_magic (marshal_string "ustar" sizeof_hdr_magic) 0 c;
        set_hdr_version (marshal_int 0 sizeof_hdr_version) 0 c;
      end else begin
        set_hdr_magic "ustar " 0 c;
        set_hdr_version (marshal_string " " sizeof_hdr_version) 0 c;
      end;
      set_hdr_uname (marshal_string x.uname sizeof_hdr_uname) 0 c;
      set_hdr_gname (marshal_string x.gname sizeof_hdr_gname) 0 c;
      if level = Ustar then begin
        set_hdr_devmajor (marshal_int x.devmajor sizeof_hdr_devmajor) 0 c;
        set_hdr_devminor (marshal_int x.devminor sizeof_hdr_devminor) 0 c;
      end
    end else begin
      if x.devmajor <> 0 then failwith "devmajor not supported in this format";
      if x.devminor <> 0 then failwith "devminor not supported in this format";
      if x.uname <> "" then failwith "uname not supported in this format";
      if x.gname <> "" then failwith "gname not supported in this format";
    end;
    set_hdr_file_mode (marshal_int x.file_mode sizeof_hdr_file_mode) 0 c;
    set_hdr_user_id   (marshal_int x.user_id sizeof_hdr_user_id) 0 c;
    set_hdr_group_id  (marshal_int x.group_id sizeof_hdr_group_id) 0 c;
    set_hdr_file_size (marshal_int64 x.file_size sizeof_hdr_file_size) 0 c;
    set_hdr_mod_time  (marshal_int64 x.mod_time sizeof_hdr_mod_time) 0 c;
    set_hdr_link_indicator c link_indicator;
    (* The caller (e.g. write_block) is expected to insert the extra ././@LongLink header *)
    if String.length x.link_name > sizeof_hdr_link_name && level <> GNU then failwith "link_name too long";
    set_hdr_link_name (marshal_string x.link_name sizeof_hdr_link_name) 0 c;
    (* Finally, compute the checksum *)
    let chksum = checksum c in
    set_hdr_chksum    (marshal_int64 chksum sizeof_hdr_chksum) 0 c

  let marshal ?level c (x: t) =
    let level = get_level level in
    imarshal ~level c (Link.to_int ~level x.link_indicator) x

  (** Thrown if we detect the end of the tar (at least two zero blocks in sequence) *)
  exception End_of_stream

  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks *)
  let compute_zero_padding_length (x: t) : int =
    (* round up to next whole number of block lengths *)
    let length = Int64.of_int length in
    let lenm1 = Int64.sub length Int64.one in
    let next_block_length = (Int64.mul length (Int64.div (Int64.add x.file_size lenm1) length)) in
    Int64.to_int (Int64.sub next_block_length x.file_size)

  (** Return the required zero-padding as a string *)
  let zero_padding (x: t) =
    let zero_padding_len = compute_zero_padding_length x in
    Cstruct.sub zero_block 0 zero_padding_len

  let to_sectors (x: t) =
    let bytes = Int64.(add x.file_size (of_int (compute_zero_padding_length x))) in
    Int64.div bytes 512L
end

let is_zero x =
  try
    for i = 0 to Cstruct.len x - 1 do
      if Cstruct.get_uint8 x i <> 0 then raise Not_found
    done;
    true
  with Not_found -> false

module type ASYNC = sig
  type 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

(* If we aren't using Lwt/Async style threads, instantiate the functor with
   this. *)
module Direct = struct
  type 'a t = 'a
  let return x = x
  let ( >>= ) m f = f m
end

module type READER = sig
  type in_channel
  type 'a t
  val really_read: in_channel -> Cstruct.t -> unit t
  val skip: in_channel -> int -> unit t
end

module type WRITER = sig
  type out_channel
  type 'a t
  val really_write: out_channel -> Cstruct.t -> unit t
end

let longlink = "././@LongLink"

module HeaderReader(Async: ASYNC)(Reader: READER with type 'a t = 'a Async.t) = struct
  open Async
  open Reader


  let read ?level (ifd: Reader.in_channel) : (Header.t, [ `Eof ]) Result.result t =
    let level = Header.get_level level in
    (* We might need to read 2 headers at once if we encounter a Pax header *)
    let buffer = Cstruct.create Header.length in
    let real_header_buf = Cstruct.create Header.length in

    let next_block () =
      really_read ifd buffer
      >>= fun () ->
      return (Header.unmarshal ~level buffer) in

    (* Skip Pax GlobalExtendedHeaders *)
    let next () =
      next_block ()
      >>= function
      | Some x when x.Header.link_indicator = Header.Link.GlobalExtendedHeader -> next_block ()
      | x -> return x in

    let get_hdr () =
      next ()
      >>= function
      | Some x when x.Header.link_indicator = Header.Link.PerFileExtendedHeader ->
        let extra_header_buf = Cstruct.create (Int64.to_int x.Header.file_size) in
        really_read ifd extra_header_buf
        >>= fun () ->
        skip ifd (Header.compute_zero_padding_length x)
        >>= fun () ->
        let extended = Header.Extended.unmarshal extra_header_buf in
        really_read ifd real_header_buf
        >>= fun () ->
        begin match Header.unmarshal ~level ~extended real_header_buf with
        | None ->
          (* Corrupt pax headers *)
          return (Result.Error `Eof)
        | Some x -> return (Result.Ok x)
        end
      | Some x when x.Header.link_indicator = Header.Link.LongLink && x.Header.file_name = longlink ->
        let extra_header_buf = Cstruct.create (Int64.to_int x.Header.file_size) in
        really_read ifd extra_header_buf
        >>= fun () ->
        skip ifd (Header.compute_zero_padding_length x)
        >>= fun () ->
        let file_name = Cstruct.(to_string @@ sub extra_header_buf 0 (len extra_header_buf - 1)) in
        begin next ()
        >>= function
        | None -> return (Result.Error `Eof)
        | Some x -> return (Result.Ok { x with file_name })
        end
      | Some x -> return (Result.Ok x)
      | None ->
        begin
          next ()
          >>= function
          | Some x -> return (Result.Ok x)
          | None -> return (Result.Error `Eof)
        end in

    let rec read_header (file_name, link_name, hdr) : (Header.t, [`Eof]) Result.result Async.t =
      let raw_link_indicator = Header.get_hdr_link_indicator buffer in
      if (raw_link_indicator = 75 || raw_link_indicator = 76) && level = Header.GNU then
        let data = Cstruct.create (Int64.to_int hdr.Header.file_size) in
        let pad = Cstruct.create (Header.compute_zero_padding_length hdr) in
        really_read ifd data
        >>= fun () ->
        really_read ifd pad
        >>= fun () ->
        let data = Header.unmarshal_string (Cstruct.to_string data) in
        get_hdr ()
        >>= function
        | Result.Error `Eof -> return (Result.Error `Eof)
        | Result.Ok hdr ->
          if raw_link_indicator = 75
          then read_header (file_name, data, hdr)
          else read_header (data, link_name, hdr)
      else begin
        let link_name = if link_name = "" then hdr.Header.link_name else link_name in
        let file_name = if file_name = "" then hdr.Header.file_name else file_name in
        return (Result.Ok {hdr with Header.link_name; file_name })
      end in
    get_hdr ()
    >>= function
    | Result.Error `Eof -> return (Result.Error `Eof)
    | Result.Ok hdr ->
      read_header ("", "", hdr)

end

module HeaderWriter(Async: ASYNC)(Writer: WRITER with type 'a t = 'a Async.t) = struct
  open Async
  open Writer
  let write_unextended ?level header fd =
    let level = Header.get_level level in
    let buffer = Cstruct.create Header.length in
    Cstruct.memset buffer 0;
    let blank = {Header.file_name = longlink; file_mode = 0; user_id = 0; group_id = 0; mod_time = 0L; file_size = 0L; link_indicator = Header.Link.LongLink; link_name = ""; uname = "root"; gname = "root"; devmajor = 0; devminor = 0; extended = None} in
    ( if (String.length header.Header.link_name > Header.sizeof_hdr_link_name || String.length header.Header.file_name > Header.sizeof_hdr_file_name) && level = Header.GNU then begin
        ( if String.length header.Header.link_name > Header.sizeof_hdr_link_name then begin
            let file_size = String.length header.Header.link_name + 1 in
            let blank = {blank with Header.file_size = Int64.of_int file_size} in
            Header.imarshal ~level buffer 75 blank;
            really_write fd buffer
            >>= fun () ->
            let text = header.Header.link_name ^ "\000" in
            let payload = Cstruct.create (String.length text) in
            Cstruct.blit_from_string text 0 payload 0 (String.length text);
            really_write fd payload
            >>= fun () ->
            really_write fd (Header.zero_padding blank)
          end else return () )
        >>= fun () ->
        ( if String.length header.Header.file_name > Header.sizeof_hdr_file_name then begin
            let file_size = String.length header.Header.file_name + 1 in
            let blank = {blank with Header.file_size = Int64.of_int file_size} in
            Header.imarshal ~level buffer 76 blank;
            really_write fd buffer
            >>= fun () ->
            let text = header.Header.file_name ^ "\000" in
            let payload = Cstruct.create (String.length text) in
            Cstruct.blit_from_string text 0 payload 0 (String.length text);
            really_write fd payload
          end else return () )
        >>= fun () ->
        Cstruct.memset buffer 0;
        return ()
      end else return () )
    >>= fun () ->
    Header.marshal ~level buffer header;
    really_write fd buffer

  let write ?level header fd =
    ( match header.Header.extended with
      | None -> return ()
      | Some e ->
        let pax_payload = Header.Extended.marshal e in
        let pax = Header.make ~link_indicator:Header.Link.PerFileExtendedHeader
          "paxheader" (Int64.of_int @@ Cstruct.len pax_payload) in
        write_unextended ?level pax fd
        >>= fun () ->
        really_write fd pax_payload
        >>= fun () ->
        really_write fd (Header.zero_padding pax) )
    >>= fun () ->
    write_unextended ?level header fd
end

module type IO = sig
  type in_channel
  type out_channel

  val really_input : in_channel -> bytes -> int -> int -> unit
  val input : in_channel -> bytes -> int -> int -> int
  val output : out_channel -> bytes -> int -> int -> unit
  val close_out : out_channel -> unit
end

module Make (IO : IO) = struct
  module Reader = struct
    type in_channel = IO.in_channel
    type 'a t = 'a Direct.t
    (* XXX: there's no function to read directly into a bigarray *)
    let really_read (ifd: IO.in_channel) buffer : unit t =
      let s = Bytes.create (Cstruct.len buffer) in
      IO.really_input ifd s 0 (Cstruct.len buffer);
      Cstruct.blit_from_bytes s 0 buffer 0 (Cstruct.len buffer)

    let skip (ifd: in_channel) (n: int) =
      let buffer = Cstruct.create 4096 in
      let rec loop (n: int) =
        if n <= 0 then ()
        else
          let amount = min n (Cstruct.len buffer) in
          really_read ifd (Cstruct.sub buffer 0 amount);
          loop (n - amount) in
      loop n
  end
  module Writer = struct
    type out_channel = IO.out_channel
    type 'a t = 'a Direct.t
    (* XXX: there's no function to write directly from a bigarray *)
    let really_write fd buffer =
      let s = Cstruct.to_string buffer |> Bytes.of_string in
      if Bytes.length s > 0
      then IO.output fd s 0 (Bytes.length s)
  end
  let really_read = Reader.really_read
  let really_write = Writer.really_write

  let finally fct clean_f =
    let result =
      try fct ();
      with exn ->
        clean_f ();
        raise exn in
    clean_f ();
    result

  module HW = HeaderWriter(Direct)(Writer)

  let write_block ?level (header: Header.t) (body: IO.out_channel -> unit) (fd : IO.out_channel) =
    HW.write ?level header fd;
    body fd;
    really_write fd (Header.zero_padding header)

  let write_end (fd: IO.out_channel) =
    really_write fd Header.zero_block;
    really_write fd Header.zero_block

  module HR = HeaderReader(Direct)(Reader)

  (** Utility functions for operating over whole tar archives *)
  module Archive = struct

    let skip = Reader.skip

    (** Read the next header, apply the function 'f' to the fd and the header. The function
        should leave the fd positioned immediately after the datablock. Finally the function
        skips past the zero padding to the next header *)
    let with_next_file (fd: IO.in_channel) (f: IO.in_channel -> Header.t -> 'a) =
      match HR.read fd with
      | Result.Ok hdr ->
        (* NB if the function 'f' fails we're boned *)
        finally (fun () -> f fd hdr)
          (fun () -> Reader.skip fd (Header.compute_zero_padding_length hdr))
      | Result.Error `Eof -> raise Header.End_of_stream

    (** List the contents of a tar *)
    let list ?level fd =
      let level = Header.get_level level in
      let list = ref [] in
      try
        while true do
          match HR.read ~level fd with
          | Result.Ok hdr ->
            list := hdr :: !list;
            Reader.skip fd (Int64.to_int hdr.Header.file_size);
            Reader.skip fd (Header.compute_zero_padding_length hdr)
          | Result.Error `Eof -> raise Header.End_of_stream
        done;
        List.rev !list;
      with
      | End_of_file -> failwith "Unexpected end of file while reading stream"
      | Header.End_of_stream -> List.rev !list

    let copy_n ifd ofd n =
      let buffer = Bytes.create 16384 in
      let rec loop remaining =
        if remaining = 0L then () else begin
          let this = Int64.(to_int (min (of_int (Bytes.length buffer)) remaining)) in
          let n = IO.input ifd buffer 0 this in
          if n = 0 then raise End_of_file;
          begin
            try
              IO.output ofd buffer 0 n
            with Failure _ -> raise End_of_file
          end;
          loop (Int64.(sub remaining (of_int n)))
        end in
      loop n

    (** [extract_gen dest] extract the contents of a tar.
        Apply 'dest' on each header to get a handle to the file to write to *)
    let extract_gen dest ifd =
      try
        while true do
          match HR.read ifd with
          | Result.Ok hdr ->
            let size = hdr.Header.file_size in
            let padding = Header.compute_zero_padding_length hdr in
            let ofd = dest hdr in
            copy_n ifd ofd size;
            IO.close_out ofd;
            Reader.skip ifd padding
          | Result.Error `Eof -> raise Header.End_of_stream
        done
      with
      | End_of_file -> failwith "Unexpected end of file while reading stream"
      | Header.End_of_stream -> ()

    (** Create a tar on file descriptor fd from the stream of headers.  *)
    let create_gen ?level files ofd =
      let level = Header.get_level level in
      let file (hdr, write) =
        write_block ~level hdr write ofd;
      in
      Stream.iter file files;
      (* Add two empty blocks *)
      write_end ofd

  end

  module Header = struct
    include Header

    let get_next_header ?level ic = match HR.read ?level ic with
      | Result.Ok hdr -> hdr
      | Result.Error `Eof -> raise Header.End_of_stream

  end
end
