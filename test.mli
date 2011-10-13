
module NoOpMonad :
  sig
    type 'a t = 'a
    val return : 'a -> 'a
    val bind : 'a -> ('a -> 'b) -> 'b
  end

module StringMonad :
  sig
    type 'a t = { data : 'a; str : string; }
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val strwr : string -> unit t
    val getstr : 'a t -> string
    val getdata : 'a t -> 'a
  end
