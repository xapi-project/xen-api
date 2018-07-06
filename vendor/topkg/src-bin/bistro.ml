(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let bistro () =
  begin
    let verb = Cmd.(v "--verbosity" % Logs.(level_to_string (level ()))) in
    let topkg = Cmd.(v "topkg") in
    OS.Cmd.run Cmd.(topkg % "distrib" %% verb)
    >>= fun () -> OS.Cmd.run Cmd.(topkg % "publish" %% verb)
    >>= fun () -> OS.Cmd.run Cmd.(topkg % "opam" %% verb % "pkg")
    >>= fun () -> OS.Cmd.run Cmd.(topkg % "opam" %% verb % "submit")
    >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let doc = "For when you are in a hurry or need to go for a drink"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [ `Main; `Cmd "distrib"; `Cmd "publish"; `Cmd "opam" ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command (quick in Russian) is equivalent to invoke:";
  `Pre "\
topkg distrib       # Create the distribution archive
topkg publish       # Publish it on the WWW with its documentation
topkg opam pkg      # Create an opam package
topkg opam submit   # Submit it to OCaml's opam repository";
    `P "See topkg-release(7) for more information."; ]

let cmd =
  Term.(pure bistro $ Cli.setup),
  Term.info "bistro" ~doc ~sdocs ~exits ~man ~man_xrefs

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
