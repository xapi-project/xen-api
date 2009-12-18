
type setup_cmd = {
  cmdargs : string list;
  env : string list;
  id_to_fd_map : (string * int option) list } 

and setup_response = {
  fd_sock_path : string } 

and process_result = 
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

and ferpc = 
    | Setup of setup_cmd
    | Setup_response of setup_response
    | Cancel 
    | Exec
    | Execed of int
    | Finished of process_result
    | Log_reopen
with rpc

