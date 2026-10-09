(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

val gen_help : unit -> unit
(** Writes the cmdlets' Get-Help content to XenServerPowerShell.dll-Help.xml.
    Fails if curated_examples.ml names a cmdlet this generator does not
    produce, so a renamed or withdrawn cmdlet cannot leave a stale example
    behind. *)
