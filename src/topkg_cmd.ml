(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Command line fragments *)

type t = string list

let empty = []
let is_empty = function [] -> true | _ -> false
let v a = [a]
let ( % ) l a = a :: l
let ( %% ) l0 l1 = List.rev_append (List.rev l1) l0
let add_arg l a = l % a
let add_args l a = l %% a
let on bool l = if bool then l else []
let p f = f

(* Predicates and comparison *)

let equal l l' = l = l'
let compare l l' = Pervasives.compare l l'

(* Conversions and pretty printing *)

let to_rev_list line = line
let to_list line = List.rev line
let of_list ?slip line = match slip with
| None -> List.rev line
| Some slip -> List.fold_left (fun acc v -> v :: slip :: acc) [] line

let dump ppf cmd =
  let pp_elt ppf s = Format.fprintf ppf "%s" (Filename.quote s) in
  let rec loop = function
  | [] -> ()
  | v :: vs ->
      if vs = [] then pp_elt ppf v else
      (Format.fprintf ppf "%a@ " pp_elt v; loop vs)
  in
  Format.fprintf ppf "@[<1>["; loop (List.rev cmd); Format.fprintf ppf "]@]"

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
