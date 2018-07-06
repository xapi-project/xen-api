(*---------------------------------------------------------------------------
   Copyright (c) 2007 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let exec = Filename.basename Sys.executable_name
let pr_err s = Printf.eprintf "%s:%s\n" exec s
let apply f x ~finally y =
  let result = try f x with exn -> finally y; raise exn in
  finally y;
  result

let fail ((l, c), e) = failwith (str "%d:%d: %s" l c (Xmlm.error_message e))

type tree = E of Xmlm.tag * tree list | D of string

let in_tree i =
  let el tag childs = E (tag, childs)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let frag = function
  | E (tag, childs) -> `El (tag, childs)
  | D d -> `Data d
  in
  Xmlm.output_doc_tree frag o t

let xml_parse tree enc strip entity ns ic () =                (* parse only *)
  let i = Xmlm.make_input ~enc ~strip ~entity ~ns (`Channel ic) in
  let doc i =
    if tree then ignore (in_tree i) else
    begin
      let rec pull i l = match Xmlm.input i with
      | `El_start _ -> pull i (l + 1)
      | `El_end -> if l = 1 then () else pull i (l - 1)
      | `Data _ -> pull i l
      | `Dtd _ -> assert false
      in
      ignore (Xmlm.input i); (* `Dtd *)
      pull i 0;
    end
  in
  try while not (Xmlm.eoi i) do doc i done
  with Xmlm.Error (p, e) -> fail (p, e)

let xml_signals _ enc strip entity ns ic _ =           (* output signals *)
  let i = Xmlm.make_input ~enc ~strip ~entity ~ns (`Channel ic) in
  let pp_signal s = Format.printf "@[%a@]@," Xmlm.pp_signal s in
  try
    Format.printf "@[<v>";
    while not (Xmlm.eoi i) do pp_signal (Xmlm.input i); done;
    Format.printf "@]";
  with Xmlm.Error (p, e) -> fail (p, e)

let xml_outline tree enc strip entity ns ic oc =            (* ascii outline *)
  let pr s = Printf.fprintf oc s in
  let pr_dtd dtd = match dtd with Some s -> pr "+-DTD %S\n" s | _ -> () in
  let pr_depth d = for k = 1 to d do pr "| " done in
  let pr_data d data = pr_depth d; pr "%S\n" data in
  let pr_name c (p, l) =  if p <> "" then pr "%s:%s" p l else pr "%s" l in
  let pr_att d (n, v) = pr_depth (d + 1); pr "* %a = %S\n" pr_name n v in
  let pr_tag d (n, atts) =
    pr_depth d; pr "+-%a\n" pr_name n; List.iter (pr_att d) atts
  in
  let i = Xmlm.make_input ~enc ~strip ~entity ~ns (`Channel ic) in
  let doc i =
    if tree then
      begin
        let rec pr_tree d = function
        | (n :: next) :: path ->
            begin match n with
            | D data -> pr_data d data; pr_tree d (next :: path)
            | E (tag, childs) ->
                pr_tag d tag; pr_tree (d+1) (childs :: next :: path)
            end
        | [] :: path -> if d = 0 then () else pr_tree (d - 1) path
        | _ -> assert false
        in
        let dtd, t = in_tree i in
        pr_dtd dtd;
        pr_tree 0 ([t] :: [])
      end
    else
    begin
      let rec pull i l = match Xmlm.input i with
      | `El_start tag -> pr_tag l tag; pull i (l + 1)
      | `El_end -> if l = 1 then () else pull i (l - 1)
      | `Data d -> pr_data l d; pull i l
      | `Dtd _ -> assert false
      in
      pr_dtd (match Xmlm.input i with `Dtd d -> d | _ -> assert false);
      pull i 0;
    end;
    flush oc
  in
  try while not (Xmlm.eoi i) do doc i done
  with Xmlm.Error (p, e) -> fail (p, e)

let xml_xml indent tree enc strip entity ns ic oc =              (* xml trip *)
  let nl = (indent = None) in
  let i = Xmlm.make_input ~enc ~strip ~ns ~entity (`Channel ic) in
  let o = Xmlm.make_output ~nl ~indent ~ns_prefix:ns (`Channel oc) in
  let doc i o =
    if tree then (out_tree o (in_tree i)) else
    begin
      let rec pull i o depth =
        let s = Xmlm.input i in
        Xmlm.output o s;
        match s with
        | `El_start _ -> pull i o (depth + 1)
        | `El_end -> if depth = 1 then () else pull i o (depth - 1)
        | `Data _ -> pull i o depth
        | `Dtd _ -> assert false
      in
      Xmlm.output o (Xmlm.input i); (* `Dtd *)
      pull i o 0
    end
  in
  try while not (Xmlm.eoi i) do doc i o done
  with Xmlm.Error (p, e) -> fail (p, e)

let with_inf f inf v =
  try
    let ic = if inf <> "" then open_in_bin inf else stdin in
    let close ic = if inf <> "" then close_in ic else () in
    apply (f ic) v ~finally:close ic
  with
  | Sys_error e -> pr_err (str " %s" e)
  | Failure e -> pr_err (str "%s:%s" inf e)

let with_outf f ic outf =
  try
    let oc = if outf <> "" then open_out_bin outf else stdout in
    let close oc = if outf <> "" then close_out oc else () in
    apply (f ic) oc ~finally:close oc
  with
  | Sys_error e -> pr_err (str " %s" e)

let entity_fun eref xhtml =
  if not xhtml then (if eref then fun x -> Some x else fun x -> None) else
  let h = Hashtbl.create 270 in
  List.iter (fun (e, ustr) -> Hashtbl.add h e ustr) Xhtml.entities;
  if eref then (fun x -> try Some (Hashtbl.find h x) with Not_found -> Some x)
  else (fun x -> try Some (Hashtbl.find h x) with Not_found -> None)

let process signals tree enc strip eref ns xhtml parse_only outline indent
    suffix files =
  let entity = entity_fun eref xhtml in
  let ns = if ns then fun x -> Some x else fun x -> None in
  let f =
    if parse_only then
      fun inf -> with_inf (xml_parse tree enc strip entity ns) inf ()
    else
    let outf inf =
      if inf = "" || suffix = "" then "" (* stdout *) else
      str "%s.%s" inf suffix
    in
    let f =
      if outline then xml_outline else
      if signals then xml_signals else
      (xml_xml indent)
    in
    fun inf ->
      with_inf (with_outf (f tree enc strip entity ns)) inf (outf inf)
  in
  List.iter f files

let encoding_of_str enc = match (String.lowercase enc) with
| "" -> None
| "utf-8" | "utf8" | "utf_8" -> Some `UTF_8
| "utf-16" | "utf16" | "utf_16" -> Some `UTF_16
| "utf-16be" | "utf16be" | "utf16_be" -> Some `UTF_16BE
| "utf-16le" | "utf16le" | "utf16_le" -> Some `UTF_16LE
| "iso-8859-1" | "iso88591"
| "iso_8859_1" | "latin1" | "latin-1" -> Some `ISO_8859_1
| "ascii" | "us-ascii" -> Some `US_ASCII
| e -> pr_err (str "unknown encoding '%s', trying to guess." e); None

let main () =
  let usage =
    str "Usage: %s <options> <files>\n\
         Reads xml files and outputs them on stdout.\n\
         Options:" exec
  in
  let enc = ref "" in
  let strip = ref false in
  let ns = ref false in
  let eref = ref false in
  let xhtml = ref false in
  let parse_only = ref false in
  let tree = ref false in
  let signals = ref false in
  let outline = ref false in
  let indent = ref false in
  let suffix = ref "" in
  let files = ref [] in
  let add_file s = files := s :: !files in
  let options = [
    "-enc", Arg.Set_string enc,
    "<enc>, use specified encoding, utf-8, utf-16, utf-16be, utf-16le,\n\
    \   iso-8859-1, ascii (otherwise guesses).";
    "-strip", Arg.Set strip,
    "strip and collapse white space in character data.";
    "-ns", Arg.Set ns,
    "replace unbound namespaces prefixes by themselves (on input and output).";
    "-eref", Arg.Set eref,
    "replace unknown entity references by their name.";
    "-xhtml", Arg.Set xhtml,
    "resolve XHTML character entities.";
    "-p", Arg.Set parse_only,
    "parse only, no output.";
    "-t", Arg.Set tree,
    "build document tree in memory.";
    "-signals", Arg.Set signals,
    "outputs the stream of signals instead of xml (excludes -t).";
    "-ot", Arg.Set outline,
    "output document ascii outline instead of xml.";
    "-indent", Arg.Set indent,
    "indent xml output.";
    "-trip", Arg.Set_string suffix,
    "<suffix>, result for file <file> is output to a file <file.suffix>."; ]
  in
  Arg.parse options add_file usage;
  let files = match (List.rev !files) with [] -> ["" (* stdin *) ] | l -> l in
  let enc = encoding_of_str !enc in
  let indent = if !indent then Some 2 else None in
  process !signals !tree enc !strip !eref !ns !xhtml !parse_only
    !outline indent !suffix files

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2007 Daniel C. Bünzli

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
