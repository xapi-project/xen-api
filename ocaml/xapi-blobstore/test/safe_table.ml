(* A safe implementation of the KV storage interface entirely in-memory.
   This is useful for checking that the property test code passes with a correct implementation.
*)

module Connection = struct
  module IO = struct type 'a t = 'a end
  
  type config = unit
  
  type t = { m: Mutex.t }
  
  let name = __MODULE__

  let pp_config _ () = ()

  let connect () = { m = Mutex.create () }
  
  let disconnect _ = ()
end