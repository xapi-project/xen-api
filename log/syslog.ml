
type level = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug
type options = Cons | Ndelay | Nowait | Odelay | Perror | Pid
type facility = Auth | Authpriv | Cron | Daemon | Ftp | Kern
              | Local0 | Local1 | Local2 | Local3
	      | Local4 | Local5 | Local6 | Local7
	      | Lpr | Mail | News | Syslog | User | Uucp

(* external init : string -> options list -> facility -> unit = "stub_openlog" *)
external log : facility -> level -> string -> unit = "stub_syslog"
external close : unit -> unit = "stub_closelog"
