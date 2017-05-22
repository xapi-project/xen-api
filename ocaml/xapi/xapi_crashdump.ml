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

let destroy ~__context ~self =
  Stdext.Pervasiveext.finally
    (Helpers.log_exn_continue
       "destroying crashdump"
       (fun ()->
          let vdi = Db.Crashdump.get_VDI ~__context ~self in
          Helpers.call_api_functions ~__context
            (fun rpc session_id ->
               Client.Client.VDI.destroy rpc session_id vdi)))
    (fun ()->
       Db.Crashdump.destroy ~__context ~self)
