(** Abstraction for blocking calls or a concurrency monad.

    This module is kept very small on purpose: it is preferred to use the
    concurrency libraries APIs directly rather than having another
    abstraction layer on top.
    
    This module is useful to be able to share type signatures for backends implemented
    using the {!mod:Unix} module, or in-memory and those implemented using {!mod:Lwt}.
*)
module type IO = sig
    type +'a t
    (** concurrency monad *)
end

(** Connection instance to a backend *)
module type Connection = sig
    module IO: IO
    type config
    (** connection configuration *)
    
    type t
    (** connection instance *)

    val connect: config -> t IO.t
    (** [connect config] establishes a connection to the backend.
        
        When the connection is no longer needed then {!val:disconnect} should be called.
        If a connection cannot be established then either the {!mod:IO} monad's builtin failure mechanism is used,
        or an exception is raised.
        
        @param config backend specific configuration (e.g. VM UUID)
        @return an {!type:IO.t} promise for a backend {!type:t} connection
    *)
    
    val disconnect: t -> unit IO.t
    (** [disconnect t] closes the connection to [t] and releases any resources.
    
        It is an error to use [t] after [disconnect] has been called.
        In particular if [disconnect] fails it mustn't be called again.
    *)
end