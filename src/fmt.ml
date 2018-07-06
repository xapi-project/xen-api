(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Errors *)

let err_str_formatter = "Format.str_formatter can't be set."

(* Formatting *)

let pf = Format.fprintf
let kpf = Format.kfprintf
let strf = Format.asprintf
let kstrf f fmt =
  let buf = Buffer.create 64 in
  let f fmt =
    Format.pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf; f s
  in
  Format.kfprintf f (Format.formatter_of_buffer buf) fmt

(* Standard output formatting *)

let stdout = Format.std_formatter
let stderr = Format.err_formatter
let pr = Format.printf
let epr = Format.eprintf

(* Exception formatting *)

let invalid_arg' = invalid_arg

let failwith fmt = kstrf failwith fmt
let invalid_arg fmt = kstrf invalid_arg fmt

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let nop fmt ppf = ()
let cut = Format.pp_print_cut
let sp = Format.pp_print_space
let comma ppf () = pf ppf ",@ "
let const pp_v v ppf () = pf ppf "%a" pp_v v
let unit fmt ppf () = pf ppf fmt
let fmt fmt ppf = pf ppf fmt
let always fmt ppf v = pf ppf fmt

(* Base type formatters *)

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let nativeint ppf v = pf ppf "%nd" v
let int32 ppf v = pf ppf "%ld" v
let int64 ppf v = pf ppf "%Ld" v
let uint ppf v = pf ppf "%u" v
let uint32 ppf v = pf ppf "%lu" v
let uint64 ppf v = pf ppf "%Lu" v
let unativeint ppf v = pf ppf "%nu" v

let char = Format.pp_print_char
let string = Format.pp_print_string
let buffer ppf b = string ppf (Buffer.contents b)

let exn ppf e = string ppf (Printexc.to_string e)
let exn_backtrace ppf (e, bt) =
  let pp_backtrace_str ppf s =
    let stop = String.length s - 1 (* there's a newline at the end *) in
    let rec loop left right =
      if right = stop then string ppf (String.sub s left (right - left)) else
      if s.[right] <> '\n' then loop left (right + 1) else
      begin
        string ppf (String.sub s left (right - left));
        cut ppf ();
        loop (right + 1) (right + 1)
      end
    in
    if s = "" then (string ppf "No backtrace available.") else
    loop 0 0
  in
  pf ppf "@[<v>Exception: %a@,%a@]"
    exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

(* Floats *)

let float ppf v = pf ppf "%g" v

let round x = floor (x +. 0.5)
let round_dfrac d x =
  if x -. (round x) = 0. then x else                   (* x is an integer. *)
  let m = 10. ** (float_of_int d) in                (* m moves 10^-d to 1. *)
  (floor ((x *. m) +. 0.5)) /. m

let round_dsig d x =
  if x = 0. then 0. else
  let m = 10. ** (floor (log10 (abs_float x))) in       (* to normalize x. *)
  (round_dfrac d (x /. m)) *. m

let float_dfrac d ppf f = pf ppf "%g" (round_dfrac d f)
let float_dsig d ppf f = pf ppf "%g" (round_dsig d f)

(* Polymorphic type formatters *)

let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
  pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

let option ?none:(pp_none = nop) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let result ~ok ~error ppf = function
| Result.Ok v -> ok ppf v
| Result.Error e -> error ppf e

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
  let is_first = ref true in
  let pp_binding k v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_binding ppf (k, v)
  in
  iter pp_binding v

let list ?sep pp_elt = iter ?sep List.iter pp_elt
let array ?sep pp_elt = iter ?sep Array.iter pp_elt
let hashtbl ?sep pp_binding = iter_bindings ?sep Hashtbl.iter pp_binding
let queue ?sep pp_elt = iter Queue.iter pp_elt
let stack ?sep pp_elt = iter Stack.iter pp_elt

let using f pp ppf v = pp ppf (f v)

module Dump = struct

  let signal ppf s = match s with
  | s when s = Sys.sigabrt -> string ppf "SIGABRT"
  | s when s = Sys.sigalrm -> string ppf "SIGALRM"
  | s when s = Sys.sigfpe -> string ppf "SIGFPE"
  | s when s = Sys.sighup -> string ppf "SIGHUP"
  | s when s = Sys.sigill -> string ppf "SIGILL"
  | s when s = Sys.sigint -> string ppf "SIGINT"
  | s when s = Sys.sigkill -> string ppf "SIGKILL"
  | s when s = Sys.sigpipe -> string ppf "SIGPIPE"
  | s when s = Sys.sigquit -> string ppf "SIGQUIT"
  | s when s = Sys.sigsegv -> string ppf "SIGSEGV"
  | s when s = Sys.sigterm -> string ppf "SIGTERM"
  | s when s = Sys.sigusr1 -> string ppf "SIGUSR1"
  | s when s = Sys.sigusr2 -> string ppf "SIGUSR2"
  | s when s = Sys.sigchld -> string ppf "SIGCHLD"
  | s when s = Sys.sigcont -> string ppf "SIGCONT"
  | s when s = Sys.sigstop -> string ppf "SIGSTOP"
  | s when s = Sys.sigtstp -> string ppf "SIGTSTP"
  | s when s = Sys.sigttin -> string ppf "SIGTTIN"
  | s when s = Sys.sigttou -> string ppf "SIGTTOU"
  | s when s = Sys.sigvtalrm -> string ppf "SIGVTALRM"
  | s when s = Sys.sigprof -> string ppf "SIGPROF"
  | unknown -> pf ppf "SIG(%d)" unknown

  let uchar ppf u = pf ppf "U+%04X" (Uchar.to_int u)

  let pair pp_fst pp_snd ppf (fst, snd) =
    pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_fst fst pp_snd snd

  let option pp_v ppf = function
  | None -> pf ppf "None"
  | Some v -> pf ppf "@[<2>Some@ @[%a@]@]" pp_v v

  let result ~ok ~error ppf = function
  | Result.Ok v -> pf ppf "@[<2>Ok@ @[%a@]@]" ok v
  | Result.Error e -> pf ppf "@[<2>Error@ @[%a@]@]" error e

  let list pp_elt ppf vs =
    let rec loop = function
    | [] -> ()
    | v :: vs ->
        if vs = [] then (pf ppf "@[%a@]" pp_elt v) else
        (pf ppf "@[%a@];@ " pp_elt v; loop vs)
    in
    pf ppf "@[<1>["; loop vs; pf ppf "]@]"

  let array pp_elt ppf a =
    pf ppf "@[<2>[|";
    for i = 0 to Array.length a - 1 do
      if i = 0 then pf ppf "@[%a@]" pp_elt a.(i) else
      pf ppf ";@ @[%a@]" pp_elt a.(i)
    done;
    pf ppf "|]@]"

  let iter iter pp_name pp_elt ppf v =
    let is_first = ref true in
    let pp_elt v =
      if !is_first then (is_first := false) else pf ppf "@ ";
      pf ppf "@[%a@]" pp_elt v
    in
    pf ppf "@[<1>(%a@ " pp_name v;
    iter pp_elt v;
    pf ppf ")@]"

  let iter_bindings iter pp_name pp_k pp_v ppf bs =
    let is_first = ref true in
    let pp_binding k v =
      if !is_first then () else pf ppf "@ ";
      pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_k k pp_v v
    in
    pf ppf "@[<1>(%a@ " pp_name bs;
    iter pp_binding bs;
    pf ppf ")@]"

  let hashtbl pp_k pp_v =
    iter_bindings Hashtbl.iter (always "hashtbl") pp_k pp_v

  let stack pp_elt = iter Stack.iter (always "stack") pp_elt
  let queue pp_elt = iter Queue.iter (always "queue") pp_elt
end

(* Boxes *)

let box ?(indent = 0) pp ppf =
  Format.pp_open_hovbox ppf indent; pf ppf "%a@]" pp

let hbox pp ppf =
  Format.pp_open_hbox ppf (); pf ppf "%a@]" pp

let vbox ?(indent = 0) pp ppf =
  Format.pp_open_vbox ppf indent; pf ppf "%a@]" pp

let hvbox ?(indent = 0) pp ppf =
  Format.pp_open_hvbox ppf indent; pf ppf "%a@]" pp

(* Brackets *)

let parens pp_v ppf v = pf ppf "@[<1>(%a)@]" pp_v v
let brackets pp_v ppf v = pf ppf "@[<1>[%a]@]" pp_v v
let braces pp_v ppf v = pf ppf "@[<1>{%a}@]" pp_v v
let quote ?(mark = "\"") pp_v ppf v =
  pf ppf "@[<1>@<1>%s%a@<1>%s@]" mark pp_v v mark

(* Text and lines *)

let is_nl c = c = '\n'
let is_nl_or_sp c = is_nl c || c = ' '
let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
let not_white c = not (is_white c)
let not_white_or_nl c = is_nl c || not_white c

let rec stop_at sat ~start ~max s =
  if start > max then start else
  if sat s.[start] then start else
  stop_at sat ~start:(start + 1) ~max s

let sub s start stop ~max =
  if start = stop then "" else
  if start = 0 && stop > max then s else
  String.sub s start (stop - start)

let words ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_white ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      match stop_at not_white ~start:stop ~max s with
      | stop when stop > max -> ()
      | stop -> Format.pp_print_space ppf (); loop stop s
  in
  let start = stop_at not_white ~start:0 ~max s in
  if start > max then () else loop start s

let paragraphs ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_white ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      match stop_at not_white_or_nl ~start:stop ~max s with
      | stop when stop > max -> ()
      | stop ->
          if s.[stop] <> '\n'
          then (Format.pp_print_space ppf (); loop stop s) else
          match stop_at not_white_or_nl ~start:(stop + 1) ~max s with
          | stop when stop > max -> ()
          | stop ->
              if s.[stop] <> '\n'
              then (Format.pp_print_space ppf (); loop stop s) else
              match stop_at not_white ~start:(stop + 1) ~max s with
              | stop when stop > max -> ()
              | stop ->
                  Format.pp_force_newline ppf ();
                  Format.pp_force_newline ppf ();
                  loop stop s
  in
  let start = stop_at not_white ~start:0 ~max s in
  if start > max then () else loop start s

let text ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl_or_sp ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      begin match s.[stop] with
      | ' ' -> Format.pp_print_space ppf ()
      | '\n' -> Format.pp_force_newline ppf ()
      | _ -> assert false
      end;
      loop (stop + 1) s
  in
  loop 0 s

let lines ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      Format.pp_force_newline ppf ();
      loop (stop + 1) s
  in
  loop 0 s

let text_loc ppf ((l0, c0), (l1, c1)) =
  if (l0 : int) == (l1 : int) && (c0 : int) == (c1 : int)
  then pf ppf "%d.%d" l0 c0
  else pf ppf "%d.%d-%d.%d" l0 c0 l1 c1

(* Appending *)

let append pp_v0 pp_v1 ppf (v0, v1) = pp_v0 ppf v0 ; pp_v1 ppf v1
let prefix pp_p pp_v ppf v = pp_p ppf (); pp_v ppf v
let suffix pp_s pp_v ppf v = pp_v ppf v; pp_s ppf ()

(* Byte sizes *)

let _pp_byte_size k i ppf s =
  let pp_frac = float_dfrac 1 in
  let div_round_up m n = (m + n - 1) / n in
  let float = float_of_int in
  if s < k then pf ppf "%dB" s else
  let m = k * k in
  if s < m then begin
    let kstr = if i = "" then "k" (* SI *) else "K" (* IEC *) in
    let sk = s / k in
    if sk < 10
    then pf ppf "%a%s%sB" pp_frac (float s /. float k) kstr i
    else pf ppf "%d%s%sB" (div_round_up s k) kstr i
  end else
  let g = k * m in
  if s < g then begin
    let sm = s / m in
    if sm < 10
    then pf ppf "%aM%sB" pp_frac (float s /. float m) i
    else pf ppf "%dM%sB" (div_round_up s m) i
  end else
  let t = k * g in
  if s < t then begin
    let sg = s / g in
    if sg < 10
    then pf ppf "%aG%sB" pp_frac (float s /. float g) i
    else pf ppf "%dG%sB" (div_round_up s g) i
  end else
  let p = k * t in
  if s < p then begin
    let st = s / t in
    if st < 10
    then pf ppf "%aT%sB" pp_frac (float s /. float t) i
    else pf ppf "%dT%sB" (div_round_up s t) i
  end else begin
    let sp = s / p in
    if sp < 10
    then pf ppf "%aP%sB" pp_frac (float s /. float p) i
    else pf ppf "%dP%sB" (div_round_up s p) i
  end

let byte_size ppf s = _pp_byte_size 1000 "" ppf s
let bi_byte_size ppf s = _pp_byte_size 1024 "i" ppf s

(* Conditional UTF-8 and styled formatting.

   This is very ugly, formally what we would like is to be able to
   store arbitrary typed metadata in formatters for clients to consult
   (tried to provide an API for doing that but dismissed it for
   uglyness and lack of an efficient implementation). In the following
   we are using the tags functions (but not the tags mechanism itself)
   as a way to store two metadata keys, one for formatter UTF-8
   awareness and the other for the formatter style renderer. *)

let utf_8_tag = "fmt.utf8"

let utf_8_of_raw = function
| "\x00" -> false
| "\x01" -> true
| _ -> true

let utf_8_to_raw = function
| false -> "\x00"
| true -> "\x01"

type style_renderer = [ `Ansi_tty | `None ]

let style_renderer_tag = "fmt.style_renderer"

let style_renderer_of_raw = function
| "\x00" -> `None
| "\x01" -> `Ansi_tty
| _ -> `None

let style_renderer_to_raw = function
| `None -> "\x00"
| `Ansi_tty -> "\x01"

let meta_store ppf = Format.pp_get_formatter_tag_functions ppf ()
let set_meta_store ppf store = Format.pp_set_formatter_tag_functions ppf store
let meta_raw store tag = store.Format.mark_open_tag tag
let set_meta ppf store ~utf_8 ~style_renderer =
  let meta = function
  | "fmt.utf8" -> utf_8
  | "fmt.style_renderer" -> style_renderer
  | _ -> "Fmt: do not use the tags mechanism, it is a broken idea"
  in
  let store = { store with Format.mark_open_tag = meta } in
  set_meta_store ppf store

let utf_8 ppf = utf_8_of_raw (meta_raw (meta_store ppf) utf_8_tag)
let set_utf_8 ppf utf_8 =
  if ppf == Format.str_formatter then invalid_arg' err_str_formatter else
  let store = meta_store ppf in
  let style_renderer = meta_raw store style_renderer_tag in
  let utf_8 = utf_8_to_raw utf_8 in
  set_meta ppf store ~utf_8 ~style_renderer

let style_renderer ppf =
  style_renderer_of_raw (meta_raw (meta_store ppf) style_renderer_tag)

let set_style_renderer ppf renderer =
  if ppf == Format.str_formatter then invalid_arg' err_str_formatter else
  let store = meta_store ppf in
  let utf_8 = meta_raw store utf_8_tag in
  let style_renderer = style_renderer_to_raw renderer in
  set_meta ppf store ~utf_8 ~style_renderer

let with_buffer ?like buf =
  let ppf = Format.formatter_of_buffer buf in
  match like with
  | None -> ppf
  | Some like ->  set_meta_store ppf (meta_store like); ppf

let strf_like ppf fmt =
  let buf = Buffer.create 64 in
  let bppf = with_buffer ~like:ppf buf in
  let flush ppf =
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    Buffer.reset buf; s
  in
  Format.kfprintf flush bppf fmt

(* Conditional UTF-8 formatting *)

let if_utf_8 pp_u pp = fun ppf v -> (if utf_8 ppf then pp_u else pp) ppf v

(* Styled formatting *)

type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White | `None ]

let ansi_style_code = function
| `Bold -> "\027[01m"
| `Underline -> "\027[04m"
| `Black -> "\027[30m"
| `Red -> "\027[31m"
| `Green -> "\027[32m"
| `Yellow -> "\027[33m"
| `Blue -> "\027[34m"
| `Magenta -> "\027[35m"
| `Cyan -> "\027[36m"
| `White -> "\027[37m"
| `None -> "\027[m"

let ansi_style_reset = "\027[m"

let styled style pp_v ppf = match style_renderer ppf with
| `None -> pp_v ppf
| `Ansi_tty ->
    let reset ppf = pf ppf "@<0>%s" ansi_style_reset in
    kpf reset ppf "@<0>%s%a" (ansi_style_code style) pp_v

let styled_unit style fmt = styled style (unit fmt)

(* Converting with string converters. *)

let of_to_string f ppf v = string ppf (f v)
let to_to_string pp_v v = strf "%a" pp_v v

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
