(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type level = App | Error | Warning | Info | Debug

let exec = match Array.length Sys.argv with
| 0 -> Filename.basename Sys.executable_name
| n -> Filename.basename Sys.argv.(0)

let _level =
  let default = Some Warning in
  let init =
    try match Sys.getenv "TOPKG_VERBOSITY" with
    | l when Topkg_string.is_prefix ~affix:"quiet" l -> None
    | l when Topkg_string.is_prefix ~affix:"error" l -> Some Error
    | l when Topkg_string.is_prefix ~affix:"warning" l -> Some Warning
    | l when Topkg_string.is_prefix ~affix:"info" l -> Some Info
    | l when Topkg_string.is_prefix ~affix:"debug" l -> Some Debug
    | l ->
        Format.eprintf
          "%s: @[TOPKG_VERBOSITY env var unknown value: %S@]@." exec l;
        default
    with Not_found | Sys_error _ -> default
  in
  ref init

let level () = !_level
let set_level l = _level := l

let level_to_string = function
| None -> "quiet" | Some App -> "app" | Some Error -> "error"
| Some Warning -> "warning" | Some Info -> "info" | Some Debug -> "debug"

let level_of_string = function
| "quiet" -> Ok None
| "app" -> Ok (Some App)
| "error" -> Ok (Some Error)
| "warning" -> Ok (Some Warning)
| "info" -> Ok (Some Info)
| "debug" -> Ok (Some Debug)
| l -> R.error_msgf "%S: unknown log level" l

type 'a msgf =
  (?header:string ->
   ('a, Format.formatter, unit) Pervasives.format -> 'a) -> unit

let _err_count = ref 0
let err_count () = !_err_count

let _warn_count = ref 0
let warn_count () = !_warn_count

let pp_level_header ppf (h,l) = match h with
| Some h -> Format.fprintf ppf "[%s] " h
| None ->
    Format.pp_print_string ppf begin match l with
    | App -> ""
    | Error -> "[ERROR] "
    | Warning -> "[WARNING] "
    | Info -> "[INFO] "
    | Debug -> "[DEBUG] "
    end

let msg level msgf = match !_level with
| None -> ()
| Some level' when level > level' ->
    if level = Error then incr _err_count else
    if level = Warning then incr _warn_count else ()
| Some _ ->
    (if level = Error then incr _err_count else
     if level = Warning then incr _warn_count else ());
    let pr = if level = App then Format.printf else Format.eprintf in
    msgf @@
    (fun ?header fmt ->
       pr ("%s: %a@[" ^^ fmt ^^ "@]@.") exec pp_level_header (header, level))

let app msgf = msg App msgf
let err msgf = msg Error msgf
let warn msgf = msg Warning msgf
let info msgf = msg Info msgf
let debug msgf = msg Debug msgf

let on_error_msg ?(level = Error) ~use = function
| Ok v -> v
| Error (`Msg e) -> msg level (fun m -> m "%s" e); use ()

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
