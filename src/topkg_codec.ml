(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Decode errors *)

type error = Corrupted of (string * string) | Version of int * int

let pp_error ppf = function
| Corrupted (kind, v) ->
    Format.fprintf ppf "corrupted %s in %S" kind v
| Version (exp, fnd) ->
    Format.fprintf ppf "version mismatch, expected %d found %d" exp fnd

exception Error of error

let err ~kind v = raise (Error (Corrupted (kind, v)))
let err_version ~exp ~fnd = raise (Error (Version (exp, fnd)))

(* Codecs *)

type 'a t =
  { kind : string;
    enc : 'a -> string;
    dec : string -> 'a; }

let v ~kind ~enc ~dec = { kind; enc; dec }
let kind c = c.kind
let enc c = c.enc
let dec c = c.dec
let with_kind kind c = { c with kind }

let dec_result c s = try Ok (dec c s) with
| Error err ->
    R.error_msgf "Decode %s: %a Input data: %S" (kind c) pp_error err s

let write file c v =
  Topkg_os.File.write file (enc c v)
  |> R.reword_error_msg ~replace:true
    (fun err -> R.msgf "Encode %s to %s: %s" (kind c) file err)

let read file c =
  Topkg_os.File.read file >>= fun s ->
  try Ok (dec c s) with
  | Error e -> R.error_msgf "Decode %s from %s: %a" (kind c) file pp_error e

(* Base type codecs *)

let tail s = Topkg_string.with_index_range ~first:1 s

let unit =
  let kind = "unit" in
  let enc = function () -> "\x00" in
  let dec = function "\x00" -> () | s -> err ~kind s in
  v ~kind ~enc ~dec

let const c =
  let kind = "const" in
  let enc = function _ -> "" in
  let dec = function "" -> c | s -> err ~kind s in
  v ~kind ~enc ~dec

let bool =
  let kind = "bool" in
  let enc = function false -> "\x00" | true -> "\x01" in
  let dec = function "\x00" -> false | "\x01" -> true | s -> err ~kind s in
  v ~kind ~enc ~dec

let int =
  let kind = "int" in
  let enc = string_of_int (* will do for now *) in
  let dec s = try int_of_string s with Failure _ -> err ~kind s in
  v ~kind ~enc ~dec

let string =
  let kind = "string" in
  let enc s = s in
  let dec s = s in
  v ~kind ~enc ~dec

let option some =
  let kind = Printf.sprintf "(%s) option" (kind some) in
  let enc = function None -> "\x00" | Some v -> "\x01" ^ (enc some v) in
  let dec s = match Topkg_string.head s with
  | Some '\x00' -> None
  | Some '\x01' -> Some (dec some (tail s))
  | _ -> err ~kind s
  in
  v ~kind ~enc ~dec

let result ~ok ~error =
  let kind = Printf.sprintf "(%s, %s) result" (kind ok) (kind error) in
  let enc = function
  | Ok v -> "\x00" ^ (enc ok v)
  | Error e -> "\x01" ^ (enc error e)
  in
  let dec s = match Topkg_string.head s with
  | Some '\x00' -> Ok (dec ok (tail s))
  | Some '\x01' -> Error (dec error (tail s))
  | _ -> err ~kind s
  in
  v ~kind ~enc ~dec

let list el =
  let kind = Printf.sprintf "(%s) list" (kind el) in
  let enc vs =
    let b = Buffer.create 255 in
    let rec loop = function
    | [] -> Buffer.add_char b '\x00'
    | v :: vs ->
      let venc = (enc el) v in
      let venc_len = String.length venc in
      Buffer.add_char b '\x01';
      Buffer.add_string b (string_of_int venc_len) (* will do for now *);
      Buffer.add_char b '\x01';
      Buffer.add_string b venc;
      loop vs
    in
    loop vs; Buffer.contents b
  in
  let dec s =
    let rec loop acc s = match Topkg_string.head s with
    | Some '\x00' -> acc
    | Some '\x01' ->
      begin match Topkg_string.find_byte ~start:1 '\x01' s with
      | None -> err ~kind s
      | Some one ->
          try
            let last = one - 1 in
            let len = Topkg_string.with_index_range ~first:1 ~last s in
            let len = int_of_string len in
            let first = one + 1 in
            let last = first + len - 1 in
            let venc = Topkg_string.with_index_range ~first ~last s in
            let rest = Topkg_string.with_index_range ~first:(last + 1) s in
            loop ((dec el venc) :: acc) rest
          with Failure _ (* of int_of_string *) -> err ~kind s
      end
    | _ -> err ~kind s
    in
    List.rev (loop [] s)
  in
  v ~kind ~enc ~dec

let seq = list string

let pair c0 c1 =
  let kind = Printf.sprintf "%s * %s" (kind c0) (kind c1) in
  let enc (v0, v1) = enc seq [enc c0 v0; enc c1 v1] in
  let dec s = match dec seq s with
  | [lenc; renc] -> (dec c0 lenc), (dec c1 renc)
  | _ -> err ~kind s
  in
  v ~kind ~enc ~dec

let t2 = pair

let t3 c0 c1 c2 =
  let kind = Printf.sprintf "%s * %s * %s" (kind c0) (kind c1) (kind c2) in
  let seq = list string in
  let enc (v0, v1, v2) = enc seq [enc c0 v0; enc c1 v1; enc c2 v2] in
  let dec s = match (dec seq) s with
  | [v0; v1; v2] -> (dec c0 v0), (dec c1 v1), (dec c2 v2)
  | _ -> err ~kind s
  in
  v ~kind ~enc ~dec

let t4 c0 c1 c2 c3 =
  let kind =
    Printf.sprintf "%s * %s * %s * %s" (kind c0) (kind c1) (kind c2) (kind c3)
  in
  let seq = list string in
  let enc (v0, v1, v2, v3) =
    enc seq [enc c0 v0; enc c1 v1; enc c2 v2; enc c3 v3]
  in
  let dec s = match (dec seq) s with
  | [v0; v1; v2; v3] -> (dec c0 v0), (dec c1 v1), (dec c2 v2), (dec c3 v3)
  | _ -> err ~kind s
  in
  v ~kind ~enc ~dec

let t5 c0 c1 c2 c3 c4 =
  let kind =
    Printf.sprintf "%s * %s * %s * %s * %s"
      (kind c0) (kind c1) (kind c2) (kind c3) (kind c4)
  in
  let seq = list string in
  let enc (v0, v1, v2, v3, v4) =
    enc seq [enc c0 v0; enc c1 v1; enc c2 v2; enc c3 v3; enc c4 v4]
  in
  let dec s = match (dec seq) s with
  | [v0; v1; v2; v3; v4] ->
      (dec c0 v0), (dec c1 v1), (dec c2 v2), (dec c3 v3), (dec c4 v4)
  | _ -> err ~kind s
  in
  v ~kind ~enc ~dec

let alt ~kind tag cs =
  let l = Array.length cs in
  if l > 256 then invalid_arg @@ Topkg_string.strf "too many codecs (%d)" l;
  let enc v =
    let tag = tag v in
    Printf.sprintf "%c%s" (Char.chr tag) (enc cs.(tag) v)
  in
  let dec s = match Topkg_string.head s with
  | None -> err ~kind s
  | Some tag ->
      let tag = Char.code tag in
      if tag < Array.length cs then dec cs.(tag) (tail s) else
      err ~kind s
  in
  v ~kind ~enc ~dec

let version version =
   let enc_version = string_of_int version in
   fun c ->
     let kind = Printf.sprintf "(%s) v%s" (kind c) enc_version in
     let enc v = String.concat "\x00" [enc_version; enc c v] in
     let dec s = match Topkg_string.cut ~sep:'\x00' s with
     | None -> err ~kind s
     | Some (fnd_version, s) ->
         try
           let fnd = int_of_string fnd_version in
           if fnd <> version then err_version ~exp:version ~fnd else
           dec c s
         with
         | Failure _ (* of int_of_string *) -> err ~kind s
     in
     v ~kind ~enc ~dec

let view ?kind:k (inj, proj) c =
  let kind = match k with None -> kind c | Some k -> k in
  let enc v = enc c (inj v) in
  let dec s = proj (dec c s) in
  v ~kind ~enc ~dec

let msg =
  let msg = (fun (`Msg m) -> m), (fun m -> `Msg m) in
  view msg string

let result_error_msg ok = result ~ok ~error:msg
let fpath = string
let cmd =
  let cmd =
    (fun cmd -> Topkg_cmd.to_list cmd),
    (fun l -> Topkg_cmd.of_list l)
  in
  view cmd (list string)

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
