(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Watermarks *)

type watermark =
  string *
  [ `String of string
  | `Name
  | `Version
  | `Version_num
  | `Vcs of [ `Commit_id ]
  | `Opam of Topkg_fpath.t option * string * string ]

let opam_fields file =
  (Topkg_opam.File.fields file)
  |> R.reword_error_msg ~replace:true (fun msg -> R.msgf "Watermarks: %s" msg)
  |> Topkg_log.on_error_msg ~level:Topkg_log.Warning ~use:(fun () -> [])

let opam_field =
  let find k m = try Some (List.assoc k m) with Not_found -> None in
  let opam_memo = ref [] in (* memoizes the opam files *)
  let rec opam_field file field = match find file !opam_memo with
  | None ->
      opam_memo := (file, (opam_fields file)) :: !opam_memo;
      opam_field file field
  | Some fields ->
      match find field fields with
      | Some vs -> vs
      | None ->
          Topkg_log.warn
            (fun m -> m "file %s: opam field %S undefined or unsupported"
                file field);
          ["UNDEFINED"]
  in
  opam_field

let vcs_commit_id () =
  (Topkg_vcs.get () >>= fun repo -> Topkg_vcs.head ~dirty:true repo)
  |> R.reword_error_msg ~replace:true
    (fun msg -> R.msgf "Watermarks: VCS commit id determination: %s" msg)
  |> Topkg_log.on_error_msg ~level:Topkg_log.Warning
    ~use:(fun () -> "UNDEFINED")

let define_watermarks ~name ~version ~opam watermarks =
  let define (id, v) =
    let (id, v as def) = match v with
    | `String s -> (id, s)
    | `Version -> (id, version)
    | `Version_num -> (id, Topkg_string.drop_initial_v version)
    | `Name -> (id, name)
    | `Vcs `Commit_id -> (id, vcs_commit_id ())
    | `Opam (file, field, sep) ->
        let file = match file with None -> opam | Some file -> file in
        (id, String.concat sep (opam_field file field))
    in
    Topkg_log.info (fun m -> m "Watermark %s = %S" id v);
    def
  in
  List.map define watermarks

let watermark_file ws file =
  Topkg_os.File.read file >>= fun content ->
  Topkg_os.File.write_subst file ws content >>= fun () ->
  Topkg_log.info (fun m -> m "Watermarked %s" file); Ok ()

let rec watermark_files ws = function
| [] -> Ok ()
| f :: fs -> watermark_file ws f >>= fun () -> watermark_files ws fs

(* Defaults *)

let default_watermarks =
  let space = " " in
  let comma = ", " in
  [ "NAME", `Name;
    "VERSION", `Version;
    "VERSION_NUM", `Version_num;
    "VCS_COMMIT_ID", `Vcs `Commit_id;
    "PKG_MAINTAINER", `Opam (None, "maintainer", comma);
    "PKG_AUTHORS", `Opam (None, "authors", comma);
    "PKG_HOMEPAGE", `Opam (None, "homepage", comma);
    "PKG_ISSUES", `Opam (None, "bug-reports", space);
    "PKG_DOC", `Opam (None, "doc", space);
    "PKG_LICENSE", `Opam (None, "license", comma);
    "PKG_REPO", `Opam (None, "dev-repo", space); ]

let default_files_to_watermark =
  let is_file f =
    Topkg_os.File.exists f |> Topkg_log.on_error_msg ~use:(fun _ -> false)
  in
  let is_binary_ext ext =
    let module Set = Set.Make (String) in
    let exts =
      Set.(empty |>
           add ".flv" |> add ".gif" |> add ".ico" |> add ".jpeg" |>
           add ".jpg" |> add ".mov" |> add ".mp3" |> add ".mp4" |>
           add ".otf" |> add ".pdf" |> add ".png" |> add ".ttf" |>
           add ".woff")
    in
    Set.mem ext exts
  in
  let keep f = not (is_binary_ext @@ Topkg_fpath.get_ext f) && is_file f in
  fun () ->
    Topkg_vcs.get ()
    >>= fun repo -> Topkg_vcs.tracked_files repo
    >>= fun files -> Ok (List.filter keep files)

let default_massage () = Ok ()

let default_exclude_paths () =
  Ok [".git"; ".gitignore"; ".gitattributes"; ".hg"; ".hgignore"; "build";
      "Makefile"; "_build"]

(* Distribution *)

type t =
  { watermarks : watermark list;
    files_to_watermark : unit -> Topkg_fpath.t list result;
    massage : unit -> unit result;
    exclude_paths : unit -> Topkg_fpath.t list result;
    uri : string option; }

let v
    ?(watermarks = default_watermarks)
    ?(files_to_watermark = default_files_to_watermark)
    ?(massage = fun () -> Ok ())
    ?(exclude_paths = default_exclude_paths)
    ?uri () =
  { watermarks; files_to_watermark; massage; exclude_paths; uri }

let watermarks d = d.watermarks
let files_to_watermark d = d.files_to_watermark
let massage d = d.massage
let exclude_paths d = d.exclude_paths
let uri d = d.uri
let codec =
  let uri = Topkg_codec.(with_kind "uri" @@ option string) in
  let fields =
    let stub () = invalid_arg "not executable outside package definition" in
    (fun d -> d.uri),
    (fun uri ->
       { watermarks = [] (* bad *); files_to_watermark = stub;
         massage = stub; exclude_paths = stub; uri })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"distrib" fields uri)

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
