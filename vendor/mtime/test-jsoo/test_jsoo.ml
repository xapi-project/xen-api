(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let setup_log () =
  let log = Dom_html.(createDiv document) in
  let add_entry s =
    let e = Dom_html.(createP document) in
    Js.Unsafe.set e "innerHTML" (Js.string s);
    Dom.appendChild log e;
  in
  Dom.appendChild (Js.Unsafe.get Dom_html.document "body") log;
  Sys_js.set_channel_flusher stdout add_entry;
  Sys_js.set_channel_flusher stderr add_entry;
  ()

let main _ =
  setup_log ();
  ignore (Tests.run ());
  Js._false

let () = Js.Unsafe.set Dom_html.window "onload" (Dom_html.handler main)


(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

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
