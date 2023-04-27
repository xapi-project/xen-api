module Connection = struct
  module IO = struct type 'a t = 'a end
  
  type config = unit
  
  type t = { m: Mutex.t }
  
  let connect () = { m = Mutex.create () }
  
  let disconnect _ = ()
end