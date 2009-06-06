
type op =        
    Cli_printer.print_fn ->
      (Xml.xml -> Xml.xml) ->
      API.ref_session -> ((string*string) list) -> unit

type imp = 
    With_fd of (Unix.file_descr -> op)
  | With_fd_local_session of (Unix.file_descr -> op)
  | No_fd of op
  | No_fd_local_session of op

(* FIXME: print warnings to the standard error channel when a user invokes a deprecated command. *)

type flag = Vm_selectors | Host_selectors | Standard | Neverforward | Hidden | Deprecated of string list

type cmd_spec =
    {reqd:string list;
     optn:string list; (* optional arguments *)
     help:string;
     implementation: imp;
     flags:flag list}


