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

(* Serves the unread part of the first block, then reads from the fd. *)
module ProbeInput = struct
  type in_channel = {
      fd: Unix.file_descr
    ; mutable peeked_buf: Cstruct.t
    ; mutable read_past_peeked: bool
    ; mutable bytes_read: int
  }

  type 'a t = 'a Identity.t

  let really_read t dest =
    let want = Cstruct.length dest in
    let from_peeked = min want (Cstruct.length t.peeked_buf) in
    if from_peeked > 0 then (
      Cstruct.blit t.peeked_buf 0 dest 0 from_peeked ;
      t.peeked_buf <- Cstruct.shift t.peeked_buf from_peeked
    ) ;
    let remaining = want - from_peeked in
    if remaining > 0 then (
      t.read_past_peeked <- true ;
      t.bytes_read <- t.bytes_read + remaining ;
      really_read t.fd (Cstruct.sub dest from_peeked remaining)
    )

  let skip t n = really_read t (Cstruct.create n)
end

module HR = Tar.HeaderReader (Identity) (ProbeInput)

let probe_header fd =
  let first = Cstruct.create Tar.Header.length in
  really_read fd first ;
  let input =
    ProbeInput.
      {
        fd
      ; peeked_buf= first
      ; read_past_peeked= false
      ; bytes_read= Tar.Header.length
      }
  in
  try
    let hdr = HR.read input |> Result.get_ok in
    Header {hdr; bytes_read= input.bytes_read}
  with _ when not input.read_past_peeked -> Not_tar first

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
