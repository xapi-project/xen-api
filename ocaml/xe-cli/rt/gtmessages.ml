type message = 
    (* Guest to Dom0 messages *)
  | CmdResult of string  

    (* Dom0 to guest messages *) 
  | CheckCD of string list  (* Check we've got connected cds *)
  | CheckVIF of string (* Check one device exists *)
  | CheckDisks of string list
  | CheckMountDisks of string list
  | SetupTestDisk of string
  | Shutdown of int
  | Reboot of int
  | Crash
  | Test
  | CheckCDFail of string list

