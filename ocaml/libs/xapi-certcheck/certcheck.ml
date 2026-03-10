(* OCaml bindings for libvarstored-certcheck.
   Type definitions must stay in sync with certcheck_stubs.c. *)

type status =
  | Ok
  | Expired
  | NotYetValid
  | ParseError
  | NoCerts
  | InternalError

type cert_info = {
  subject          : string;
  issuer           : string;
  not_before       : int64;
  not_after        : int64;
  is_expired       : bool;
  is_not_yet_valid : bool;
  variable_name    : string;
}

external check_expiration : bytes -> status
  = "caml_varstored_check_cert_expiration"

external check_expiration_at : bytes -> int64 -> status
  = "caml_varstored_check_cert_expiration_at"

external get_cert_info : bytes -> status * cert_info list
  = "caml_varstored_get_cert_info"

external status_string : status -> string
  = "caml_varstored_cert_status_string"
