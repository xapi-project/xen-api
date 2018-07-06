(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Command *)

let clean_args name build_dir =
  let on_some_use_opt opt to_arg = function
  | None -> Cmd.empty
  | Some value -> Cmd.(v opt % to_arg value)
  in
  let verb = Cli.propagate_verbosity_to_pkg_file () in
  let build_dir = on_some_use_opt "--build-dir" Cmd.p build_dir in
  let name = on_some_use_opt "--pkg-name" (fun n -> n) name in
  Cmd.(verb %% name %% build_dir)

let clean () pkg_file name build_dir =
  let pkg = Topkg_care.Pkg.v ?build_dir ?name pkg_file in
  let args = clean_args name build_dir in
  let out = OS.Cmd.out_stdout in
  begin
    OS.Dir.current ()
    >>= fun dir -> Topkg_care.Pkg.clean pkg ~dir ~args ~out
    >>| (function ((), (_, `Exited 0)) -> 0 | _ -> 1)
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let doc = "Clean the package's build"
let sdocs = Manpage.s_common_options
let exits = Term.exit_info 1 ~doc:"on clean failure." :: Cli.exits
let man_xrefs = [`Main; `Cmd "build"]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]...";
    `S Manpage.s_description;
    `P "The $(tname) command deletes the package's build and its opam
        install file. This is equivalent to invoke:";
    `Pre "ocaml ./pkg/pkg.ml clean";]

let cmd =
  Term.(pure clean $ Cli.setup $ Cli.pkg_file $ Cli.pkg_name $ Cli.build_dir),
  Term.info "clean" ~doc ~sdocs ~exits ~man ~man_xrefs

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
