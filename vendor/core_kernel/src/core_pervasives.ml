module Core_pervasives = struct
  include Pervasives

  (* At Jane Street, the OCaml stdlib is patched to define [Pervasives.raise] as the
     ["%reraise"] primitive. We do this as the compiler is currently not good enough at
     automatically detecting reraise [1]. We patch the stdlib so that everything is
     affected, including libraries defined before base such as sexplib or non Jane Street
     libraries.

     We need this definition so that this implementation can match its interface with the
     patched stdlib and with the original one.

     [[1] http://caml.inria.fr/mantis/view.php?id=6556
  *)
  external raise : exn -> 'a = "%reraise"
end

include Core_pervasives

(* See core_pervasives.ml for details *)
module Modified_INRIA_pervasives = struct
  include Pervasives
  external raise : exn -> 'a = "%reraise"
end

(* This is here just to assert that the interfaces match, so we'll notice when INRIA
   changes Pervasives. *)
include ((Core_pervasives           : module type of Modified_INRIA_pervasives) : sig end)
include ((Modified_INRIA_pervasives : module type of Core_pervasives)           : sig end)
