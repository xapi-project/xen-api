(** OCaml bindings for libvarstored-certcheck.

    Provides access to the certificate expiration checking functions in
    varstored's NVRAM data. *)

(** Status codes returned by certificate check functions. *)
type status =
  | Ok             (** All certificates are valid *)
  | Expired        (** At least one certificate is expired *)
  | NotYetValid    (** At least one certificate is not yet valid *)
  | ParseError     (** Error parsing NVRAM data *)
  | NoCerts        (** No certificates found in NVRAM *)
  | InternalError  (** Internal error (e.g. memory allocation failure) *)

(** Information about a single X.509 certificate found in the NVRAM. *)
type cert_info = {
  subject        : string;  (** Certificate subject DN *)
  issuer         : string;  (** Certificate issuer DN *)
  not_before     : int64;   (** Validity start (Unix timestamp) *)
  not_after      : int64;   (** Validity end   (Unix timestamp) *)
  is_expired     : bool;    (** True if the certificate has expired *)
  is_not_yet_valid : bool;  (** True if the certificate is not yet valid *)
  variable_name  : string;  (** EFI variable name (e.g. "PK", "KEK", "db") *)
}

(** [check_expiration data] checks whether any certificate in the serialised
    NVRAM blob [data] is expired at the current system time. *)
val check_expiration : bytes -> status

(** [check_expiration_at data time] is like {!check_expiration} but checks
    expiration against [time] (a Unix timestamp) instead of the current time. *)
val check_expiration_at : bytes -> int64 -> status

(** [get_cert_info data] returns the overall status together with a list of
    {!cert_info} records for every X.509 certificate found in [data]. *)
val get_cert_info : bytes -> status * cert_info list

(** [status_string s] returns a human-readable description of [s]. *)
val status_string : status -> string
