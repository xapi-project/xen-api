(*
   Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

let introduce ~__context ~uuid ~vM ~profile ~contents =
  let ref = Ref.make () in
  Db.VTPM.create ~__context ~ref ~uuid ~vM ~profile ~contents ;
  ref

let create ~__context ~vM =
  let uuid = Uuid.(to_string (make ())) in
  let profile = Db.VM.get_default_vtpm_profile ~__context ~self:vM in
  let contents = Xapi_secret.create ~__context ~value:"" ~other_config:[] in
  let ref = introduce ~__context ~uuid ~vM ~profile ~contents in
  ref

let destroy ~__context ~self =
  let secret = Db.VTPM.get_contents ~__context ~self in
  Db.Secret.destroy ~__context ~self:secret ;
  Db.VTPM.destroy ~__context ~self

let get_contents ~__context ~self =
  let secret = Db.VTPM.get_contents ~__context ~self in
  Base64.decode_exn (Db.Secret.get_value ~__context ~self:secret)

let set_contents ~__context ~self ~contents =
  let previous_secret = Db.VTPM.get_contents ~__context ~self in
  let encoded = Base64.encode_exn contents in
  let secret = Xapi_secret.create ~__context ~value:encoded ~other_config:[] in
  Db.VTPM.set_contents ~__context ~self ~value:secret ;
  Db.Secret.destroy ~__context ~self:previous_secret
