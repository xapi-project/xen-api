module Identity = struct
  type 'a t = 'a

  let return x = x

  let ( >>= ) m f = f m
end

module Writer = struct
  type out_channel = Unix.file_descr

  type 'a t = 'a Identity.t

  let really_write fd buffer =
    let s = Cstruct.to_string buffer in
    ignore (Unix.write_substring fd s 0 (String.length s))
end

module HW = Tar.HeaderWriter (Identity) (Writer)

let rec with_restart op fd buf off len =
  try op fd buf off len
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    (with_restart [@tailcall]) op fd buf off len

let rec _really_input fd buf off = function
  | 0 ->
      ()
  | len ->
      let m = Unix.read fd buf off len in
      if m = 0 then raise End_of_file ;
      _really_input fd buf (off + m) (len - m)

let really_input = with_restart _really_input

let really_read ifd buffer =
  let s = Bytes.create (Cstruct.length buffer) in
  really_input ifd s 0 (Cstruct.length buffer) ;
  Cstruct.blit_from_bytes s 0 buffer 0 (Cstruct.length buffer)

let skip ifd n =
  let buffer = Cstruct.create 4096 in
  let rec loop n =
    if n <= 0 then
      ()
    else
      let amount = min n (Cstruct.length buffer) in
      really_read ifd (Cstruct.sub buffer 0 amount) ;
      loop (n - amount)
  in
  loop n

type probe =
  | Header of {hdr: Tar.Header.t; bytes_read: int}
  | Not_tar of Cstruct.t

(* Serves the first block once, then reads from the fd. *)
module ProbeInput = struct
  type in_channel = {
      fd: Unix.file_descr
    ; mutable first_block: Cstruct.t option
          (* [None] once the tar reader has taken it *)
    ; mutable bytes_read: int
  }

  type 'a t = 'a Identity.t

  let really_read t dest =
    match t.first_block with
    | Some first_block ->
        t.first_block <- None ;
        (* Tar.HeaderReader reads the first header as one whole block. *)
        assert (Cstruct.length dest = Cstruct.length first_block) ;
        Cstruct.blit first_block 0 dest 0 (Cstruct.length first_block)
    | None ->
        t.bytes_read <- t.bytes_read + Cstruct.length dest ;
        really_read t.fd dest

  let skip t n = really_read t (Cstruct.create n)
end

module HR = Tar.HeaderReader (Identity) (ProbeInput)

let probe_header fd =
  let first_block = Cstruct.create Tar.Header.length in
  really_read fd first_block ;
  match Tar.Header.unmarshal first_block with
  | (exception _) | None ->
      Not_tar first_block
  | Some _ -> (
      let input =
        ProbeInput.
          {fd; first_block= Some first_block; bytes_read= Tar.Header.length}
      in
      match HR.read input with
      | Ok hdr ->
          Header {hdr; bytes_read= input.bytes_read}
      | Error `Eof ->
          raise End_of_file
    )

let copy_n ifd ofd n =
  let buffer = Bytes.create 65536 in
  let rec loop remaining =
    if remaining = 0L then
      ()
    else
      let this =
        Int64.(to_int (min (of_int (Bytes.length buffer)) remaining))
      in
      let n = Unix.read ifd buffer 0 this in
      if n = 0 then raise End_of_file ;
      ignore (Unix.write ofd buffer 0 n) ;
      loop Int64.(sub remaining (of_int n))
  in
  loop n

let write_block hdr write_fn fd =
  HW.write hdr fd ;
  write_fn fd ;
  Writer.really_write fd (Tar.Header.zero_padding hdr)

let write_end fd =
  Writer.really_write fd Tar.Header.zero_block ;
  Writer.really_write fd Tar.Header.zero_block
