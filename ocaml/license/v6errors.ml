type error = string * string list with rpc
exception Error of error

let to_string = function
  | Error (name, args) ->
    Printf.sprintf "V6error(%s, [ %a ])" name (fun () -> String.concat "; ") args
  | e -> Printexc.to_string e


let invalid_edition = "INVALID_EDITION"
let v6d_failure = "V6D_FAILURE"

let license_expired = "LICENSE_EXPIRED"
let license_processing_error = "LICENSE_PROCESSING_ERROR"
let missing_connection_details = "MISSING_CONNECTION_DETAILS"
let license_checkout_error = "LICENSE_CHECKOUT_ERROR"
let license_file_deprecated = "LICENSE_FILE_DEPRECATED"
let activation_while_not_free = "ACTIVATION_WHILE_NOT_FREE"

