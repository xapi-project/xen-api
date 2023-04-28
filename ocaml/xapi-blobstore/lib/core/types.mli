(** Abstraction for blocking calls or a concurrency monad.

    This module is kept very small on purpose:
    backends are expected to use only one concurrency library and its full features,
    rather than functorizing over an IO monad and being restricted in what APIs it can use.
    
    This module is useful to be able to share type signatures for backends implemented
    using the {!mod:Unix} module, or in-memory and those implemented using {!mod:Lwt}.
    
    For testing purposes a functor is provided that turns a direct implementation into an Lwt one.
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
    
    val name: string
    (** [name] the backend's name. It is recommended to use __MODULE__ for this. *)
    
    val pp_config: config Fmt.t
    (** [pp formatter config] pretty prints [config] on [formatter] for debugging purposes. *)

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

(** A key-value storage backend.
*)
module type S = sig
    include Connection
end

(** A key-value storage backend in direct style (without a concurrency library). *)
module type KVDirect = S with type 'a IO.t = 'a

(** A key-value storage backend using {!mod:Lwt} for concurrency. *)
module type KVLwt = S with type 'a IO.t = 'a Lwt.t