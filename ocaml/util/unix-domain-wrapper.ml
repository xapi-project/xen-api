(* ------------------------------------------------------------------

   Copyright (c) 2006 Xensource Inc

   Contacts: Dave Scott    <david.scott@xensource.com>

   Listens on a Unix domain socket; accepts connections; forks
   child process; rebinds stdin/stdout to the client socket; execs
   a command.

   Useful to handling stunnel on Unix machines.

   ------------------------------------------------------------------ *)

let socket = ref ""
let args = ref []

let _ = 
   Arg.parse [ "-socket", Arg.Set_string socket, "listen on socket";
               "--", Arg.Rest (fun x -> args := x :: !args), "Sub-command to execute" ]
   (fun x -> Printf.eprintf "ignoring unknown argument: %s\n" x)
   "Listen on a Unix domain socket, fork subprocess with stdin/stdout rebound";
   if !args = [] || !socket = "" then begin
     Printf.eprintf "Usage:\n";
     Printf.eprintf "%s <filename> [arg0] [arg1] ... [argn]\n" Sys.argv.(0);
     Printf.eprintf "   -- listens for connections to Unix domain socket, runs sub-command for each accept\n";
     exit(-1)
   end;
   let args = List.rev !args in
   Printf.eprintf "sub-command = [ %s ]\n" (String.concat "; " args);
   begin try
     Unix.unlink !socket;
   with Unix.Unix_error(_,_,_) -> ()
   end;
   let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
   Unix.bind s (Unix.ADDR_UNIX !socket);
   Unix.listen s 5;
    
   while true do
     let _, _, _ = Unix.select [ s ] [] [] (-1.) in
     let client, addr = Unix.accept s in
     Printf.eprintf "accepted\n";
     (* NB double-fork *)
     match Unix.fork() with
     | 0 ->
        if Unix.fork () <> 0 then exit 0;
        Unix.close s;
        Unix.dup2 client Unix.stdin;
        Unix.dup2 client Unix.stdout;
        Unix.execvp (List.hd args) (Array.of_list args);
     | child ->
        Unix.close client;
        ignore(Unix.waitpid [] child)
   done
