module L = Debug.Debugger(struct let name="license" end)
open Vmopshelpers
open Stringext

let vm ~__context vm =
  (* Here we check that the license is still valid - this should be the only place where this happens *)
  if not (License.license_valid ()) then raise (Api_errors.Server_error (Api_errors.license_expired, []))

(* XXX: why use a "with_" style function here? *)
let with_vm_license_check ~__context v f =
  vm ~__context v;
  f()
