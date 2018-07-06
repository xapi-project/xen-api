(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* opam File *)

module File = struct
  type t = (string * string list) list

  let codec =
    Topkg_codec.version 0 @@
    Topkg_codec.with_kind "opam fields" @@
    Topkg_codec.(list (pair string (list string)))

  let topkg_cmd = Topkg_cmd.v "topkg"
  let topkg_cmd_available () =
    Topkg_os.Cmd.must_exist topkg_cmd
    |> R.reword_error_msg ~replace:true
      (fun m -> R.msgf "%s. Did you install topkg-care ?" m)

  let ipc_cmd file =
    (* Propagate the log level to the IPC call *)
    let level = Topkg_log.(level_to_string (level ())) in
    let verbosity = Topkg_string.strf "--verbosity=%s" level in
    Topkg_cmd.(v "ipc" % verbosity % "opam-fields" % file)

  let fields file =
    begin
      let cmd = Topkg_cmd.(topkg_cmd %% ipc_cmd file) in
      topkg_cmd_available ()
      >>= fun _ -> Topkg_os.File.must_exist file
      >>= fun _ -> Topkg_os.Cmd.(run_out cmd |> to_string)
      >>= fun s -> (Topkg_codec.dec_result codec s)
    end
    |> R.reword_error_msg ~replace:true
      (fun msg -> R.msgf "opam fields of %s: %s" file msg)
end

(* opam install file *)

module Install = struct

  type field =
  [ `Bin
  | `Doc
  | `Etc
  | `Lib
  | `Lib_root
  | `Libexec
  | `Libexec_root
  | `Man
  | `Misc
  | `Sbin
  | `Share
  | `Share_root
  | `Stublibs
  | `Toplevel
  | `Unknown of string ]

  let field_to_string = function
  | `Bin -> "bin"
  | `Doc -> "doc"
  | `Etc -> "etc"
  | `Lib -> "lib"
  | `Lib_root -> "lib_root"
  | `Libexec -> "libexec"
  | `Libexec_root -> "libexec_root"
  | `Man -> "man"
  | `Misc -> "misc"
  | `Sbin -> "sbin"
  | `Share -> "share"
  | `Share_root -> "share_root"
  | `Stublibs -> "stublibs"
  | `Toplevel -> "toplevel"
  | `Unknown name -> name

  type move = { src : string; dst : string option; maybe : bool; }

  let move ?(maybe = false) ?dst src = { src; dst; maybe }

  type t = [ `Header of string option ] * (field * move) list

  let to_string (`Header h, mvs) =
    let b = Buffer.create 1024 in
    let pr b fmt = Printf.bprintf b fmt in
    let pr_header b = function None -> () | Some h -> pr b "# %s\n\n" h in
    let pr_src b src maybe =
      pr b "  \"%s%s\"" (if maybe then "?" else "") src
    in
    let pr_dst b dst = match dst with
    | None -> ()
    | Some dst -> pr b " {\"%s\"}" dst
    in
    let pr_field_end b last = if last <> "" (* not start *) then pr b " ]\n" in
    let pr_field b last field =
      if last = field then pr b "\n" else
      (pr_field_end b last; pr b "%s: [\n" field)
    in
    let pr_move b last (field, { src; dst; maybe }) =
      pr_field b last field;
      pr_src b src maybe;
      pr_dst b dst;
      field
    in
    let sortable (field, mv) = (field_to_string field, mv) in
    let moves = List.(sort compare (rev_map sortable mvs)) in
    pr_header b h;
    let last = List.fold_left (pr_move b) "" moves in
    pr_field_end b last;
    Buffer.contents b
end

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
