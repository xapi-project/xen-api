(* Disable "Warning 39: unused rec flag." caused by rpc *)
[@@@warning "-39"]

type syslog_stdout = {enabled: bool; key: string option} [@@deriving rpc]

type setup_cmd = {
    cmdargs: string list
  ; env: string list
  ; id_to_fd_map: (string * int option) list
  ; syslog_stdout: syslog_stdout
  ; redirect_stderr_to_stdout: bool
}
[@@deriving rpc]

type setup_response = {fd_sock_path: string} [@@deriving rpc]

type process_result = WEXITED of int | WSIGNALED of int | WSTOPPED of int
[@@deriving rpc]

type ferpc =
  | Setup of setup_cmd
  | Setup_response of setup_response
  | Exec
  | Execed of int
  | Finished of process_result
  | Dontwaitpid
[@@deriving rpc]

let ferpc_to_string ferpc = rpc_of_ferpc ferpc |> Rpclib.Rpclib.to_string

(* Re-enable "Warning 39: unused rec flag." *)
[@@@warning "+39"]
