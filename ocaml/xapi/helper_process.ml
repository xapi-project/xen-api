(* Hopefully, this handler will be able to look at the return codes of the cmds, and produce *)
(* a reasonable error. For now, just return the error code! *)
let generic_handler cmd n =
  raise (Api_errors.Server_error (Api_errors.internal_error, [string_of_int n]))
    
let get_process_output ?(handler=generic_handler) cmd = Unixext.get_process_output ~handler cmd
