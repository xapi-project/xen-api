(** Guarantee to read 'n' bytes from a file descriptor or raise End_of_file *)
let really_read (fd: Unix.file_descr) n = 
  let buffer = String.make n '\000' in
  let rec really_read off n =
    if n=0 then buffer else
      let m = Unix.read fd buffer off n in
      if m = 0 then raise End_of_file;
      really_read (off+m) (n-m)
  in
  really_read 0 n

let unmarshal_uint8 (s, offset) =
  int_of_char s.[offset], (s, offset+1)

let unmarshal_uint16 ?(bigendian=false) (s, offset) =
  let offsets = if bigendian then [|1;0|] else [|0;1|] in
  let (<!<) a b = a lsl b 
  and (|!|) a b = a lor b in
  let a = int_of_char (s.[offset + offsets.(0)]) 
  and b = int_of_char (s.[offset + offsets.(1)]) in
  (a <!< 0) |!| (b <!< 8), (s, offset + 2)

let unmarshal_uint32 ?(bigendian=false) (s, offset) = 
  let offsets = if bigendian then [|3;2;1;0|] else [|0;1;2;3|] in
  let (<!<) a b = Int32.shift_left a b 
  and (|!|) a b = Int32.logor a b in
  let a = Int32.of_int (int_of_char (s.[offset + offsets.(0)])) 
  and b = Int32.of_int (int_of_char (s.[offset + offsets.(1)])) 
  and c = Int32.of_int (int_of_char (s.[offset + offsets.(2)])) 
  and d = Int32.of_int (int_of_char (s.[offset + offsets.(3)])) in
  (a <!< 0) |!| (b <!< 8) |!| (c <!< 16) |!| (d <!< 24), (s, offset + 4)

let unmarshal_uint64 ?(bigendian=false) (s, offset) = 
  let offsets = if bigendian then [|7;6;5;4;3;2;1;0|] else [|0;1;2;3;4;5;6;7|] in
  let (<!<) a b = Int64.shift_left a b 
  and (|!|) a b = Int64.logor a b in
  let a = Int64.of_int (int_of_char (s.[offset + offsets.(0)])) 
  and b = Int64.of_int (int_of_char (s.[offset + offsets.(1)])) 
  and c = Int64.of_int (int_of_char (s.[offset + offsets.(2)])) 
  and d = Int64.of_int (int_of_char (s.[offset + offsets.(3)])) 
  and e = Int64.of_int (int_of_char (s.[offset + offsets.(4)])) 
  and f = Int64.of_int (int_of_char (s.[offset + offsets.(5)])) 
  and g = Int64.of_int (int_of_char (s.[offset + offsets.(6)])) 
  and h = Int64.of_int (int_of_char (s.[offset + offsets.(7)])) in
  (a <!< 0) |!| (b <!< 8) |!| (c <!< 16) |!| (d <!< 24)
  |!| (e <!< 32) |!| (f <!< 40) |!| (g <!< 48) |!| (h <!< 56), (s, offset + 8)

let unmarshal_string len (s,offset) =
  String.sub s offset len, (s, offset + len)

let skip len (s,offset) =
  (s,offset+len)

let marshal_int8 (s,offset) x =
  s.[offset] <- char_of_int x;
  (s,offset+1)

let marshal_int16 (s,offset) ?(bigendian=false) x = 
  let offsets = if bigendian then [|1;0|] else [|0;1|] in
  let (>!>) a b = a lsr b
  and (&&) a b = a land b in
  let a = (x >!> 0) && 0xff 
  and b = (x >!> 8) && 0xff in
  s.[offset+offsets.(0)] <- char_of_int a;
  s.[offset+offsets.(1)] <- char_of_int b;
  (s,offset+2)

let marshal_int32 (s,offset) ?(bigendian=false) x = 
  let offsets = if bigendian then [|3;2;1;0|] else [|0;1;2;3|] in
  let (>!>) a b = Int32.shift_right_logical a b
  and (&&) a b = Int32.logand a b in
  let a = (x >!> 0) && 0xffl 
  and b = (x >!> 8) && 0xffl
  and c = (x >!> 16) && 0xffl
  and d = (x >!> 24) && 0xffl in
  s.[offset+offsets.(0)] <- char_of_int (Int32.to_int a);
  s.[offset+offsets.(1)] <- char_of_int (Int32.to_int b);
  s.[offset+offsets.(2)] <- char_of_int (Int32.to_int c);
  s.[offset+offsets.(3)] <- char_of_int (Int32.to_int d);
  (s,offset+4)

let marshal_int64 (s,offset) ?(bigendian=false) x = 
  let offsets = if bigendian then [|7;6;5;4;3;2;1;0|] else [|0;1;2;3;4;5;6;7|] in
  let (>!>) a b = Int64.shift_right_logical a b
  and (&&) a b = Int64.logand a b in
  let a = (x >!> 0) && 0xffL
  and b = (x >!> 8) && 0xffL
  and c = (x >!> 16) && 0xffL
  and d = (x >!> 24) && 0xffL 
  and e = (x >!> 32) && 0xffL 
  and f = (x >!> 40) && 0xffL
  and g = (x >!> 48) && 0xffL
  and h = (x >!> 56) && 0xffL in
  s.[offset+offsets.(0)] <- char_of_int (Int64.to_int a);
  s.[offset+offsets.(1)] <- char_of_int (Int64.to_int b);
  s.[offset+offsets.(2)] <- char_of_int (Int64.to_int c);
  s.[offset+offsets.(3)] <- char_of_int (Int64.to_int d);
  s.[offset+offsets.(4)] <- char_of_int (Int64.to_int e);
  s.[offset+offsets.(5)] <- char_of_int (Int64.to_int f);
  s.[offset+offsets.(6)] <- char_of_int (Int64.to_int g);
  s.[offset+offsets.(7)] <- char_of_int (Int64.to_int h);
  (s,offset+8)

let marshal_string (s,offset) x =
  let l = String.length x in
  String.blit x 0 s offset l;
  (s,offset+l)
