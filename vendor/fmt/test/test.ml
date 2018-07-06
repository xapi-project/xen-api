(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let test_exn_backtrace () = (* Don't move this test in the file. *)
  try failwith "Test" with
  | ex ->
      let bt = Printexc.get_raw_backtrace () in
      let fmt = Fmt.strf "%a" Fmt.exn_backtrace (ex,bt) in
      assert begin match Printexc.backtrace_status () with
      | false -> fmt = "Exception: Failure(\"Test\")\nNo backtrace available."
      | true ->
          fmt = "Exception: Failure(\"Test\")\n\
                 Raised at file \"pervasives.ml\", line 32, characters 22-33\n\
                 Called from file \"test/test.ml\", line 8, characters 6-21"
      end

let test_dump_uchar () =
 let str u = Format.asprintf "%a" Fmt.Dump.uchar u in
 assert (str Uchar.min = "U+0000");
 assert (str Uchar.(succ min) = "U+0001");
 assert (str Uchar.(of_int 0xFFFF) = "U+FFFF");
 assert (str Uchar.(succ (of_int 0xFFFF)) = "U+10000");
 assert (str Uchar.(pred max) = "U+10FFFE");
 assert (str Uchar.max = "U+10FFFF");
 ()

let test_utf_8 () =
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.utf_8 ppf = true);
  Fmt.set_utf_8 ppf false;
  assert (Fmt.utf_8 ppf = false);
  Fmt.set_utf_8 ppf true;
  assert (Fmt.utf_8 ppf = true);
  ()

let test_style_renderer () =
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.style_renderer ppf = `None);
  Fmt.set_style_renderer ppf `Ansi_tty;
  assert (Fmt.style_renderer ppf = `Ansi_tty);
  Fmt.set_style_renderer ppf `None;
  assert (Fmt.style_renderer ppf = `None);
  ()

let test_exn_typechecks () =
  let (_ : bool) = true || Fmt.failwith "%s" "" in
  let (_ : bool) = true || Fmt.invalid_arg "%s" "" in
  ()

let test_kstrf_strf_like_partial_app () =
  let assertf f = assert (f "X" = f "X") in
  let test_kstrf fmt = Fmt.kstrf (fun x -> x) fmt in
  let test_strf_like fmt = Fmt.strf_like Fmt.stderr fmt in
  assertf (test_strf_like "%s");
  assertf (test_kstrf "%s");
  ()


let tests () =
  test_exn_backtrace ();
  test_dump_uchar ();
  test_utf_8 ();
  test_style_renderer ();
  test_kstrf_strf_like_partial_app ();
  Printf.printf "Done.\n";
  ()

let () = tests ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
