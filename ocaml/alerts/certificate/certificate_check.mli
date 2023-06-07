type cert =
  | CA of API.ref_Certificate * API.datetime
  | Host of API.ref_host * API.datetime
  | Internal of API.ref_host * API.datetime

val generate_alert : float -> cert -> cert * (string * (string * int64)) option

include Lib.Alerter
