module type Set =
  sig
    type t
    val ( + ) : t -> t -> t
    val ( ^ ) : t -> t -> t
    val ( - ) : t -> t -> t
    val not : t -> t
    val ( = ) : t -> t -> bool
    val to_string : t -> string
  end
module SetEqualities :
  functor (S : Set) ->
    sig
      val all : (S.t -> S.t -> S.t -> unit) list
    end
