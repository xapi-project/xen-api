(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type ext = [ `Ext of string | `Obj | `Lib | `Dll | `Exe ]
type t = ext list

let interface = [ `Ext ".mli"; `Ext ".cmi"; `Ext ".cmti"; ]
let cmx = [ `Ext ".cmx" ]
let api = interface @ cmx
let c_library = [ `Lib ]
let c_dll_library = [ `Dll ]
let library = [` Ext ".cma"; `Ext ".cmxa"; `Ext ".cmxs" ] @ c_library
let module_library = (api @ library)
let exe = [ `Exe ]
let ext e = [ `Ext e ]
let exts es = List.map (fun e -> `Ext e) es

let ext_to_string c =
  let ext_obj = Topkg_conf.OCaml.ext_obj c in
  let ext_lib = Topkg_conf.OCaml.ext_lib c in
  let ext_dll = Topkg_conf.OCaml.ext_dll c in
  let ext_exe = Topkg_conf.OCaml.ext_exe c in
  function
  | `Ext s -> s
  | `Obj -> ext_obj
  | `Lib -> ext_lib
  | `Dll -> ext_dll
  | `Exe -> ext_exe

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

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
