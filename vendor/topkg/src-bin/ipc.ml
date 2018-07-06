(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let opam_fields file =
  begin
    Logs.info begin fun m ->
      m ~header:"IPC" "opam fields of %s with cwd %a" file
        (R.pp ~ok:Fpath.pp ~error:R.pp_msg) (OS.Dir.current ())
    end;
    Fpath.of_string file
    >>= fun file -> Topkg_care.Opam.File.fields file
    >>= fun fs -> Ok (String.Map.bindings fs)
    >>= fun fs -> Ok (Topkg.Private.(Codec.enc Opam.File.codec fs))
    >>= fun enc -> OS.File.(write dash enc)
  end
  |> R.reword_error_msg ~replace:true
    (fun msg -> R.msgf "ipc opam-fields: %s" msg)

let ipc_answer = function
| ["opam-fields"; file] -> opam_fields file
| args -> R.error_msgf "ipc: unknown arguments %a" Cmd.dump (Cmd.of_list args)

let ipc () args = match ipc_answer args with
| Ok () -> `Ok 0
| Error (`Msg msg) -> `Error (false, msg)

(* Command line interface *)

open Cmdliner

let args =
  let doc = "IPC call arguments" in
  Arg.(value (pos_all string [] & info [] ~doc ~docv:"ARG"))

let doc = "Interprocess communication with package description files"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [`Main]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command is used by package description files. It is
        undocumented." ]

let cmd =
  Term.(ret (pure ipc $ Cli.setup $ args)),
  Term.info "ipc" ~doc ~sdocs ~exits ~man ~man_xrefs

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
