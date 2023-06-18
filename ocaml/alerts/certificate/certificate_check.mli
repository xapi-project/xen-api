val alert : (Rpc.call -> Rpc.response) -> [< `session] Ref.t -> unit

(* Below exposed only for ease of testing *)

type cert =
  | CA of API.ref_Certificate * API.datetime
  | Host of API.ref_host * API.datetime
  | Internal of API.ref_host * API.datetime

val certificate_description : cert -> string

val message_sent_on_remaining_days_list : cert -> (int * (string * int64)) list

val get_expiry : cert -> Xapi_stdext_date.Date.t

val generate_alert :
     Xapi_stdext_date.Date.t
  -> string
  -> (int * (string * int64)) list
  -> Xapi_stdext_date.Date.t
  -> (string * (string * int64)) option
