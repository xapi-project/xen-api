(*
 * Copyright (c) 2016 Hannes Mehnert <hannes@mehnert.org>
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

[%%cstruct
type foo = {
  a : uint8_t;
  b : uint16_t;
  c : uint32_t;
  d : uint8_t [@len 8]
} [@@big_endian]
]

[%%cstruct
type bar = {
  a : uint8_t;
  b : uint16_t;
  c : uint32_t;
  d : uint8_t [@len 8]
} [@@big_endian]
]

[%%cstruct
type lbar = {
  a : uint8_t;
  b : uint16_t;
  c : uint32_t;
  d : uint8_t [@len 8]
} [@@little_endian]
]

(* see #72
[%%cstruct
type hbar = {
  a : uint8_t;
  b : uint16_t;
  c : uint32_t;
  d : uint8_t [@len 8]
} [@@host_endian]
]
*)
let tests () =
  (* Test basic set/get functions *)
  let be = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_foo)) in
  for i = 0 to 255 do
    set_bar_a be i;
    assert(get_bar_a be = i)
  done;
  let le = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_bar)) in
  for i = 0 to 255 do
    set_foo_a le i;
    assert(get_foo_a le = i)
  done;
  let be = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_foo)) in
  for i = 0 to 65535 do
    set_bar_b be i;
    assert(get_bar_b be = i)
  done;
  let le = Cstruct.of_bigarray(Bigarray.(Array1.create char c_layout sizeof_bar)) in
  for i = 0 to 65535 do
    set_foo_b le i;
    assert(get_foo_b le = i)
  done;
  let be = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_foo)) in
  let rec fn = function
   |i when i < 0l -> ()
   |i ->
      set_bar_c be i;
      assert(get_bar_c be = i);
      fn (Int32.sub i 0x10l)
  in fn 0xffffffff_l;
  (* Get/set buffers and blits *)
  let le = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_bar)) in
  let rec fn = function
   |i when i < 0l -> ()
   |i ->
      set_foo_c le i;
      assert(get_foo_c le = i);
      fn (Int32.sub i 0x10l)
  in fn 0xffffffff_l;
  let s1 = "deadbeef" in
  set_foo_d s1 0 be;
  assert(copy_foo_d be = s1);
  let sb1 = get_foo_d be in
  blit_bar_d sb1 0 le;
  assert(copy_bar_d le = s1);
  Printf.printf "%s %s\n" (copy_foo_d be) (copy_bar_d le);
  (* Create sub-view and shift it back *)
  let be = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_foo)) in
  set_foo_a be 7;
  set_foo_b be 44;
  set_foo_c be 0xbeef_l;
  set_foo_d "abcdefgh" 0 be;
  (* get a subview *)
  let be2 = Cstruct.shift be 3 in
  assert(Cstruct.BE.get_uint32 be2 0 = 0xbeef_l);
  assert(Cstruct.BE.get_uint32 be 3 = 0xbeef_l);
  assert(get_foo_b be = 44);
  assert(get_foo_a be = 7);
  hexdump_foo be;
  print_endline (Sexplib.Sexp.to_string_hum (Cstruct.sexp_of_t be))
