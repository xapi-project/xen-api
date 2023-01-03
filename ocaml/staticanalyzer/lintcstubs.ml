(* goblint.ml is a copy of the one in goblint itself
   (it is not exposed in a library, although most of its functionality is
   exposed through Maingoblint)

   We'll link any custom analysis by using dune's linkall features:
   they will register themselves on startup.
*)

let main = Goblint.main
