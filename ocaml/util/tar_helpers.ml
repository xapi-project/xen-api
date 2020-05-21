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
module HW = Tar.HeaderWriter(Identity)(Writer)

let rec with_restart op fd buf off len =
  try
    op fd buf off len
  with Unix.Unix_error (Unix.EINTR,_,_) ->
    (with_restart [@tailcall]) op fd buf off len
  

let rec _really_input fd buf off = function
  | 0 -> ()
  | len ->
    let m = Unix.read fd buf off len in
    if m = 0 then (
      raise End_of_file
    );
    _really_input fd buf (off+m) (len-m)

let really_input = with_restart _really_input

let really_read ifd buffer =
  let s = Bytes.create (Cstruct.len buffer) in
  really_input ifd s 0 (Cstruct.len buffer);
  Cstruct.blit_from_bytes s 0 buffer 0 (Cstruct.len buffer)

let skip ifd n =
  let buffer = Cstruct.create 4096 in
  let rec loop n =
    if n <= 0 then ()
    else (
      let amount = min n (Cstruct.len buffer) in
      really_read ifd (Cstruct.sub buffer 0 amount);
      loop (n - amount)
    )
  in
  loop n

let copy_n ifd ofd n =
  let buffer = Bytes.create 65536 in
  let rec loop remaining =
  if remaining = 0L then ()
  else (
    let this = Int64.(to_int (min (of_int (Bytes.length buffer)) remaining)) in
    let n = Unix.read ifd buffer 0 this in
    if n = 0 then 
      raise End_of_file;
    ignore (Unix.write ofd buffer 0 n);    
    loop (Int64.(sub remaining (of_int n)))
  )
  in
  loop n

let write_block hdr write_fn fd =
  HW.write hdr fd;
  write_fn fd;
  Writer.really_write fd (Tar.Header.zero_padding hdr)

let write_end fd =
  Writer.really_write fd Tar.Header.zero_block;
  Writer.really_write fd Tar.Header.zero_block;
