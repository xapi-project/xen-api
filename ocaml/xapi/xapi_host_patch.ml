open Pervasiveext
open Stringext
open Unixext
open Http
open Forkhelpers

module D = Debug.Debugger(struct let name="xapi" end)
open D

let destroy ~__context ~self = 
  Db.Host_patch.destroy ~__context ~self

let apply ~__context ~self =
  raise (Api_errors.Server_error (Api_errors.message_deprecated, 
                                  []))
