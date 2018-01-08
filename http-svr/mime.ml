(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* MIME handling for HTTP responses *)

open Printf

(** Map extension to MIME type *)
type t = (string, string) Hashtbl.t

let lowercase = Astring.String.Ascii.lowercase

(** Parse an Apache-format mime.types file and return mime_t *)
let mime_of_file file =
  let h = Hashtbl.create 1024 in
  Xapi_stdext_unix.Unixext.readfile_line (fun line ->
      if not (Astring.String.is_prefix ~affix:"#" line) then begin
        match Astring.String.fields line with
        | [] | [_] -> ()
        | mime::exts ->
          List.iter (fun e ->
              Hashtbl.add h (lowercase e) mime
            ) exts
      end
    ) file;
  h

let string_of_mime m =
  String.concat "," (Hashtbl.fold (fun k v a ->
      sprintf "{%s:%s}" k v :: a) m [])

let default_mime = "text/plain"

(** Map a file extension to a MIME type *)
let mime_of_ext mime ext =
  try Hashtbl.find mime (lowercase ext)
  with Not_found -> default_mime

(** Figure out a mime type from a full filename *)
let mime_of_file_name mime fname =
  (* split filename into dot components *)
  let ext = match Astring.String.cuts ~sep:"." fname with
    | [] | [_] -> ""
    | x -> List.hd (List.rev x) in
  mime_of_ext mime ext

