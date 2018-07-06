
(** Encodes a string using the given translation function that maps a character
    to a string that is its encoded version, if that character needs encoding. *)
let encode translate s =
  let n = String.length s in
  let need_encoding =
    let b = ref false in
    let i = ref 0 in
    while not !b && !i < n do
      b := translate s.[ !i ] <> None;
      incr i;
    done;
    !b in
  if need_encoding then begin
    let buf = Buffer.create 0 in
    let m = ref 0 in
    for i = 0 to n-1 do
      match translate s.[i] with
      | None   -> ()
      | Some n ->
        Buffer.add_substring buf s !m (i - !m);
        Buffer.add_string buf n;
        m := i + 1
    done;
    Buffer.add_substring buf s !m (n - !m);
    Buffer.contents buf
  end else
    s

