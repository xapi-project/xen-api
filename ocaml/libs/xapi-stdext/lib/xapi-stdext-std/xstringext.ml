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
module String = struct
  include String

  let of_char c = String.make 1 c

  let rev_map f string =
    let n = length string in
    String.init n (fun i -> f string.[n - i - 1])

  let rev_iter f string =
    for i = length string - 1 downto 0 do
      f string.[i]
    done

  let fold_left f accu string =
    let accu = ref accu in
    for i = 0 to length string - 1 do
      accu := f !accu string.[i]
    done ;
    !accu

  let fold_right f string accu =
    let accu = ref accu in
    for i = length string - 1 downto 0 do
      accu := f string.[i] !accu
    done ;
    !accu

  (** Returns true for whitespace characters, false otherwise *)
  let isspace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

  let split_f p str =
    let split_one seq =
      let not_p c = not (p c) in
      let a = Seq.take_while not_p seq in
      let b = Seq.drop_while not_p seq in
      (a, b)
    in
    let drop seq = Seq.drop_while p seq in
    let rec split acc chars =
      if Seq.is_empty chars then
        acc
      else
        let a, b = split_one chars in
        let b = drop b in
        let acc = if Seq.is_empty a then acc else Seq.cons a acc in
        split acc b
    in
    String.to_seq str
    |> split Seq.empty
    |> Seq.map String.of_seq
    |> List.of_seq
    |> List.rev

  let index_opt s c =
    let rec loop i =
      if String.length s = i then
        None
      else if s.[i] = c then
        Some i
      else
        loop (i + 1)
    in
    loop 0

  let rec split ?(limit = -1) c s =
    let i = match index_opt s c with Some x -> x | None -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0 then
      [s]
    else
      let a = String.sub s 0 i
      and b = String.sub s (i + 1) (String.length s - i - 1) in
      a :: split ~limit:nlimit c b

  let rtrim s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\n' then
      String.sub s 0 (n - 1)
    else
      s

  (** has_substr str sub returns true if sub is a substring of str. Simple, naive, slow. *)
  let has_substr str sub =
    if String.length sub > String.length str then
      false
    else
      let result = ref false in
      for start = 0 to String.length str - String.length sub do
        if String.sub str start (String.length sub) = sub then result := true
      done ;
      !result

  (** find all occurences of needle in haystack and return all their respective index *)
  let find_all needle haystack =
    let m = String.length needle and n = String.length haystack in
    if m > n then
      []
    else
      let i = ref 0 and found = ref [] in
      while !i < n - m + 1 do
        if String.sub haystack !i m = needle then (
          found := !i :: !found ;
          i := !i + m
        ) else
          incr i
      done ;
      List.rev !found

  (* replace all @f substring in @s by @t *)
  let replace f t s =
    let indexes = find_all f s in
    let n = List.length indexes in
    if n > 0 then (
      let len_f = String.length f and len_t = String.length t in
      let new_len = String.length s + (n * len_t) - (n * len_f) in
      let new_b = Bytes.make new_len '\000' in
      let orig_offset = ref 0 and dest_offset = ref 0 in
      List.iter
        (fun h ->
          let len = h - !orig_offset in
          Bytes.blit_string s !orig_offset new_b !dest_offset len ;
          Bytes.blit_string t 0 new_b (!dest_offset + len) len_t ;
          orig_offset := !orig_offset + len + len_f ;
          dest_offset := !dest_offset + len + len_t
        )
        indexes ;
      Bytes.blit_string s !orig_offset new_b !dest_offset
        (String.length s - !orig_offset) ;
      Bytes.unsafe_to_string new_b
    ) else
      s

  let filter_chars s valid =
    let badchars = ref false in
    let buf = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      if !badchars then (
        if valid s.[i] then
          Buffer.add_char buf s.[i]
      ) else if not (valid s.[i]) then (
        Buffer.add_substring buf s 0 i ;
        badchars := true
      )
    done ;
    if !badchars then Buffer.contents buf else s

  let map_unlikely s f =
    let changed = ref false in
    let m = ref 0 in
    let buf = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      match f s.[i] with
      | None ->
          ()
      | Some n ->
          changed := true ;
          Buffer.add_substring buf s !m (i - !m) ;
          Buffer.add_string buf n ;
          m := i + 1
    done ;
    if !changed then (
      Buffer.add_substring buf s !m (String.length s - !m) ;
      Buffer.contents buf
    ) else
      s

  let escaped ?rules s =
    match rules with
    | None ->
        String.escaped s
    | Some rules ->
        map_unlikely s (fun c -> List.assoc_opt c rules)

  let sub_to_end s start =
    let length = String.length s in
    String.sub s start (length - start)

  let sub_before c s = String.sub s 0 (String.index s c)

  let sub_after c s = sub_to_end s (String.index s c + 1)
end
