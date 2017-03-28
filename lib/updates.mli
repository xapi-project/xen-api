module D : Debug.DEBUG

module type INTERFACE =
  sig
    val service_name : string
    module Dynamic :
      sig type id val rpc_of_id : id -> Rpc.t val id_of_rpc : Rpc.t -> id end
  end

module Updates :
  functor (Interface : INTERFACE) ->
    sig
      type id = int
      type t
      val empty : Scheduler.t -> t
      val last_id : 'a -> t -> int
      type get_result = (int * Interface.Dynamic.id list) list * Interface.Dynamic.id list * id
      val get :
        string ->
        ?with_cancel:((unit -> unit) ->
                      (unit -> get_result) -> get_result) ->
        id option ->
        int option -> t -> get_result

      val add : Interface.Dynamic.id -> t -> unit
      val remove : Interface.Dynamic.id -> t -> unit
      val filter : (Interface.Dynamic.id -> bool) -> t -> unit
      val inject_barrier : int -> (Interface.Dynamic.id -> int -> bool) -> t -> unit
      val remove_barrier : int -> t -> unit

      module Dump :
        sig
          type u = { id : int; v : string; }
          type dump = {
            updates : u list;
            barriers : (int * int * u list) list;
          }
          val rpc_of_dump : dump -> Rpc.t
          val dump_of_rpc : Rpc.t -> dump
          val make : t -> dump
        end

    end
