(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

type flavour = [ `Markdown | `Asciidoc ]

let flavour_of_fpath f = match String.Ascii.lowercase (Fpath.get_ext f) with
| ".md" -> Some `Markdown
| ".asciidoc" | ".adoc" -> Some `Asciidoc
| _ -> None

let rec drop_blanks = function "" :: ls -> drop_blanks ls | ls -> ls
let last_line = function [] -> None | l :: rev_ls -> Some l

(* Detecting headers *)

let simple_header hchar l before rest =
  match String.(length @@ take ~sat:(Char.equal hchar) l) with
  | 0 -> None
  | n -> Some (n, l, before, rest)

let underline_header n uchar l before rest =
  let is_underline_header uchar l =
    String.(length @@ take ~sat:(Char.equal uchar) l) >= 2
  in
  if not (is_underline_header uchar l) then None else
  match last_line before with
  | None -> None
  | Some t -> Some (n, strf "%s\n%s" t l, List.tl before, rest)

let rec find_markdown_header before = function
| [] -> None
| l :: ls ->
    match simple_header '#' l before ls with
    | Some _ as h -> h
    | None ->
        match underline_header 1 '=' l before ls with
        | Some _ as h -> h
        | None ->
            match underline_header 2 '-' l before ls with
            | Some _ as h -> h
            | None -> find_markdown_header (l :: before) ls

let rec find_asciidoc_header before = function
| [] -> None
| l :: ls ->
    match simple_header '=' l before ls with
    | Some _ as h -> h
    | None ->
        match underline_header 1 '-' l before ls with
        | Some _ as h -> h
        | None ->
            match underline_header 2 '~' l before ls with
            | Some _ as h -> h
            | None ->
                match underline_header 3 '^' l before ls with
                | Some _ as h -> h
                | None ->
                    match underline_header 4 '+' l before ls with
                    | Some _ as h -> h
                    | None -> find_asciidoc_header (l :: before) ls

let head find_header text =
  let lines = String.cuts ~sep:"\n" text in
  let ret h acc =
    let contents = String.concat ~sep:"\n" (List.rev @@ drop_blanks acc) in
    Some (h, contents)
  in
  match find_header [] lines with
  | None -> None
  | Some (n, first, _ (* discard *), rest) ->
      let rec loop acc rest = match find_header acc rest with
      | None -> ret first (List.rev_append rest acc)
      | Some (n', h, before, rest) ->
          if n' > n then loop (h :: before) rest else
          ret first before
      in
      loop [] rest

let head ?(flavour = `Markdown) text = match flavour with
| `Markdown -> head find_markdown_header text
| `Asciidoc -> head find_asciidoc_header text

let header_title ?(flavour = `Markdown) h = match String.cuts ~sep:"\n" h with
| [h] ->
    begin match flavour with
    | `Markdown -> String.(trim @@ drop ~sat:(Char.equal '#') h)
    | `Asciidoc -> String.(trim @@ drop ~sat:(Char.equal '=') h)
    end
| h :: _  -> h (* underline headers *)
| [] -> assert false

(* Toy change log parsing *)

let change_log_last_entry ?flavour text = match head ?flavour text with
| None -> None
| Some (h, changes) ->
    let title = header_title ?flavour h in
    match String.take ~sat:Char.Ascii.is_graphic title with
    | "" -> Logs.app (fun m -> m "%S %S" h changes);   None
    | version -> Some (version, (h, changes))

let change_log_file_last_entry file =
  let flavour = flavour_of_fpath file in
  OS.File.read file
  >>= fun text -> match change_log_last_entry ?flavour text with
  | None -> R.error_msgf "%a: Could not parse change log." Fpath.pp file
  | Some (version, (header, changes)) -> Ok (version, (header, changes))

(* Toy URI parsing *)

let split_uri ?(rel = false) uri = match String.(cut ~sep:"//" (trim uri)) with
| None -> None
| Some (scheme, rest) ->
    match String.cut ~sep:"/" rest with
    | None -> Some (scheme, rest, "")
    | Some (host, path) ->
        let path = if rel then path else "/" ^ path in
        Some (scheme, host, path)

(* Edit and page text *)

let find_pager ~don't =
  if don't then Ok None else
  match OS.Env.var "TERM" with
  | Some "dumb" | None -> Ok None
  | _ ->
      let add_env v cmds = match OS.Env.(value v (some cmd) ~absent:None) with
      | None -> cmds
      | Some cmd -> cmd :: cmds
      in
      let cmds = [Cmd.v "less"; Cmd.v "more" ] in
      let cmds = add_env "PAGER" cmds in
      let rec loop = function
      | [] -> Ok None
      | cmd :: cmds ->
          OS.Cmd.exists cmd >>= function
          | true -> Ok (Some cmd)
          | false -> loop cmds
      in
      loop cmds

let edit_file f = match OS.Env.(value "EDITOR" (some cmd) ~absent:None) with
| None -> R.error_msg "EDITOR environment variable undefined."
| Some editor ->
    OS.Cmd.exists editor >>= function
    | false -> R.error_msgf "Editor %a not in search path" Cmd.pp editor
    | true ->
        OS.Cmd.(run_status Cmd.(editor % p f)) >>= function
        | `Exited n | `Signaled n -> Ok n

(* Pretty-printers. *)

module Pp = struct
  let name = Fmt.(styled `Bold string)
  let version = Fmt.(styled `Cyan string)
  let commit = Fmt.(styled `Yellow string)
  let dirty = Fmt.(styled_unit `Red "dirty")
  let path = Fmt.(styled `Bold Fpath.pp)
  let status ppf = function
  | `Ok -> Fmt.(brackets @@ styled_unit `Green " OK ") ppf ()
  | `Fail -> Fmt.(brackets @@ styled_unit `Red "FAIL") ppf ()
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
