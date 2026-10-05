(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(** Worked examples for the cmdlets where the generated one-liner does not
    teach enough. See curated_examples.ml for what belongs here and for the
    checks that keep the entries honest. *)

val examples : (string * (string * string * string) list) list
(** Cmdlet name, then (title, code, explanation) triples, in the order they
    should be shown. *)

val for_cmdlet : string -> (string * string * string) list
(** The examples for a cmdlet.
    @param name - Cmdlet to look up.
    @return Its examples; [] when the generated example is enough. *)

val cmdlet_names : string list
(** Every cmdlet named in [examples]. gen_powershell_help uses this to refuse
    to build when an entry names a cmdlet it does not generate. *)
