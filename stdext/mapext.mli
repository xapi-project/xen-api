module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val mem:  key -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val fromHash : (key, 'a) Hashtbl.t -> 'a t
    val filter : ('a -> bool) -> 'a t -> 'a t

    (* values: gives the list of values of the map. *)
    val values : 'a t -> 'a list

    val fromListWith : ('a -> 'a -> 'a) -> (key * 'a) list -> 'a t
    (* Update a value at a specific key with the result of the
    provided function. When the key is not a member of the map, the
    original map is returned. *)
    val adjust : ('a -> 'a) -> key -> 'a t -> 'a t
  end

module Make (Ord : Map.OrderedType) : S with type key = Ord.t
