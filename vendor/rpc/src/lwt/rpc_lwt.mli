type lwt_rpcfn = Rpc.call -> Rpc.response Lwt.t

module M :
  sig
    type 'a lwt = { lwt : 'a Lwt.t; }
    type ('a, 'b) t = ('a, 'b) Result.result lwt
    val return : 'a -> ('a, 'b) t
    val return_err : 'b -> ('a, 'b) t
    val checked_bind : ('a, 'b) t -> ('a -> ('c, 'd) t) -> ('b -> ('c, 'd) t) -> ('c, 'd) t
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val lwt : 'a lwt -> 'a Lwt.t
  end

module GenClient () :
  sig
    type implementation = unit
    val implement : Idl.Interface.description -> implementation
    exception MarshalError of string
    type ('a,'b) comp = ('a,'b) Result.result M.lwt
    type rpcfn = Rpc.call -> Rpc.response Lwt.t
    type 'a res = rpcfn -> 'a
    type _ fn =
        Function : 'a Idl.Param.t * 'b fn -> ('a -> 'b) fn
      | Returning : ('a Idl.Param.t * 'b Idl.Error.t) -> ('a, 'b) M.t fn
    val returning : 'a Idl.Param.t -> 'b Idl.Error.t -> ('a, 'b) M.t fn
    val ( @-> ) : 'a Idl.Param.t -> 'b fn -> ('a -> 'b) fn
    val declare : string -> string list -> 'a fn -> rpcfn -> 'a
  end

exception MarshalError of string
exception UnknownMethod of string
exception UnboundImplementation of string list

type server_implementation
val server : server_implementation -> lwt_rpcfn
val combine : server_implementation list -> server_implementation

module GenServer () :
  sig
    type implementation = server_implementation
    val implement : Idl.Interface.description -> implementation
    type ('a,'b) comp = ('a,'b) Result.result M.lwt
    type rpcfn = Rpc.call -> Rpc.response Lwt.t
    type funcs = (string, rpcfn option) Hashtbl.t
    type 'a res = 'a -> unit
    type _ fn =
        Function : 'a Idl.Param.t * 'b fn -> ('a -> 'b) fn
      | Returning : ('a Idl.Param.t * 'b Idl.Error.t) -> ('a, 'b) M.t fn
    val returning : 'a Idl.Param.t -> 'b Idl.Error.t -> ('a, 'b) M.t fn
    val ( @-> ) : 'a Idl.Param.t -> 'b fn -> ('a -> 'b) fn
    val declare : string -> string list -> 'a fn -> 'a res
  end
