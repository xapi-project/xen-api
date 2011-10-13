
module Wsprotocol :
  functor (IO : Iteratees.MonadIO) ->
    sig 
      module I:
        sig
	  type 'a t = 'a Iteratees.Iteratee(IO).t
          val writer : (string -> 'a IO.t) -> unit t
        end
      val base64encode : 'a I.t -> 'a I.t I.t
      val base64decode : 'a I.t -> 'a I.t I.t
      val wsframe : 'a I.t -> 'a I.t I.t
      val wsunframe : 'a I.t -> 'a I.t I.t
    end


val runtest : unit -> unit
