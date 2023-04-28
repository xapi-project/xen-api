(**
    An abstract key-value store.

    Keys and values have different types to make it clearer in the code
    what refers to keys and what refers to values (if they're both strings code could mistakenly return
    a key when a value is expected).
    They may also have different limits and stored (encoded) differently internally.
*)

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
module type BoundedString = sig
    (** the type for a string with a maximum length. May be stored encoded in a backend specific way. *)
    type t
    (*@
        ephemeral
        mutable model keys: Key.t set
        mutable model view: Key.t -> Value.t option
        invariant forall k: Key.t. not (Set.mem k keys) -> view k = None
        invariant forall k: Key.t. view k = None -> not (Set.mem k keys)
        invariant Set.cardinal keys <= max_key_count
        invariant Set.fold (fun k acc -> kv_length k (view k) + acc) keys 0 <= max_data_size
    *)
  
    val max_length : int
    (** [max_length] is the maximum length of [t], inclusive. *)
    (*@ ensures max_length > 0 *)
  
    val of_string_exn : string -> t
    (** [of_string_exn s] creates a {!type:t} value, performing any encoding as necessary.
  
    It may contain arbitrary byte values.
    Backends mustn't assume that keys are valid ASCII, or UTF-8 and perform their own encoding/decoding if necessary.
    An empty string is a valid value.
  
    @raises Invalid_argument if [s] is longer than {!val:max_length}
    *)
    (*@ t = of_string_exn s
        checks (String.length s < max_length)
    *)
  
    val to_string : t -> string
    (** [to_string t] returns [t] as a string, performing any decoding as necessary. *)
    (*@ ensures forall s. String.length s <= max_length -> String.equal s (to_string (of_string_exn s)) *)
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
    (*@ consumes t *)
end

(** Key-value store.

  An implementation of this module type is called a backend.

  Backends impose various limits:
    * on key length, see {!val:Key.max_length}
    * on value lengths, see {!val:Value.max_length}
    * on numbers of keys stored in the backend, see {!val:max_key_count}
    * on total amount of data stored in the backend, see {!val:max_data_size}

  However they  must accept any bytes values [0, 255] as a key or a value.
  If needed the backend should perform its own encoding and decoding,
  and shouldn't assume keys or values are valid UTF-8 or ASCII.

  These limits are the same for all APIs and specific to an instance {!type:t} of a backend.

  For example if the configuration is a VM UUID, then a different VM may be considered a different backend
  instance with its own independent limit counting.
*)
module type S = sig
  include Connection

  (** Keys that are valid to be stored in the backend *)
  module Key : BoundedString

  (** Values that are valid to be stored in the backend *)
  module Value : BoundedString

  val max_key_count : int
  (** [max_key_count] is the maximum number of keys in the backend, inclusive.

    This may be temporarily exceeded when executing concurrent {!val:put} calls,
    however when all {!val:put} calls finish the number of keys will be <= max_key_count.
  *)
  (*@ ensures max_key_count > 0 *)

  val max_data_size : int
  (** [max_data_size] is the total maximum amount of data in the backend, inclusive.

    This may be temporarily exceeded when executing concurrent {!val:put} calls,
    however when all {!val:put} calls finish the number of keys will be <= max_data_size.

    The amount of data stored for a key-value pair is defined as:
    [String.length (Key.to_string k) + String.length (Value.to_string v)].
    If this would be too much for the backend (e.g. due to encoding or other overheads) then it should lower [max_data_size] accordingly.
  *)
  (*@ ensures max_data_size > 0 *)

  (*@ function kv_length(k: Key.t) (v: Value.t) = String.length (Key.to_string k) + String.length (Value.to_string v) *)

  val get : t -> Key.t -> Value.t option IO.t
  (** [get conn key] retrieves the value for [key] if any.

      @param conn backend connection
      @param key the {!type:Key.t} to look up.
      @return [Some value] if [key] is present with [value], otherwise [None]
  *)
  (*@ r = get t k
      pure
      ensures r = t.view k *)

  val put : t -> Key.t -> Value.t -> unit IO.t
  (** [put conn key value] sets [key=value].

    @param conn backend connection
    @param key the {!type:Key.t} to set
    @param value the {!type:Value.t} to set
    @return a resolved [io] promise when the key has been set
  *)
  (*@
    put t key value
    requires Set.fold (fun k acc -> kv_length k (view k) + acc) keys (kv_length key value) <= max_data_size
    modifies t
    ensures get t key = Some value
    ensures t.keys = Set.add key (old t.keys)
    ensures forall k. t.view k = if k = key then Some value else old (t.view k)
  *)

  val delete : t -> Key.t -> unit IO.t
  (** [delete conn key] deletes [key].
        It is not an error if they key is already missing.

    @param conn backend connection
    @param key the {!type:Key.t} to delete
    @return a resolved [io] promise when the key has been deleted
  *)
  (*@
    delete t key
    modifies t
    ensures get t key = None
    ensures t.keys = Set.remove key (old t.keys)
    ensures forall k. t.view k = if k = key then None else old (t.view k)
  *)

  val list : t -> Key.t list IO.t
  (** [list conn] returns a list of all keys in the backend.
        The order in which keys are returned is unspecified.

    @param conn backend connection
    @return list of keys in unspecified order
  *)
  (*@ l = list t
      pure
      ensures List.length l <= max_key_count
      ensures Set.of_list l = keys
  *)
end

(** A key-value storage backend in direct style (without a concurrency library). *)
module type KVDirect = S with type 'a IO.t = 'a

(** A key-value storage backend using {!mod:Lwt} for concurrency. *)
module type KVLwt = S with type 'a IO.t = 'a Lwt.t