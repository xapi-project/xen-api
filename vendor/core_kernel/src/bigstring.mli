(** String type based on [Bigarray], for use in I/O and C-bindings. *)

open! Import


open Bigarray

(** {2 Types and exceptions} *)


(** Type of bigstrings *)
type t = (char, int8_unsigned_elt, c_layout) Array1.t
[@@deriving bin_io, compare, sexp]

(** Type of bigstrings which support hashing. Note that mutation invalidates previous hashes. *)
type t_frozen = t [@@deriving bin_io, compare, hash, sexp]

include Equal.S   with type t := t
include Hexdump.S with type t := t

(** {2 Creation and string conversion} *)

val create : ?max_mem_waiting_gc:Byte_units.t -> int -> t
(** [create length]
    @param max_mem_waiting_gc default = 256 M in OCaml <= 3.12, 1 G otherwise. As
    the total allocation of calls to [create] approach [max_mem_waiting_gc],
    the pressure in the garbage collector to be more agressive will increase.
    @return a new bigstring having [length].
    Content is undefined. *)

(** [init n ~f] creates a bigstring [t] of length [n], with [t.{i} = f i] *)
val init : int -> f:(int -> char) -> t

val of_string : ?pos : int -> ?len : int -> string -> t
(** [of_string ?pos ?len str] @return a new bigstring that is equivalent
    to the substring of length [len] in [str] starting at position [pos].

    @param pos default = 0
    @param len default = [String.length str - pos] *)

val of_bytes : ?pos : int -> ?len : int -> bytes -> t
(** [of_bytes ?pos ?len str] @return a new bigstring that is equivalent
    to the subbytes of length [len] in [str] starting at position [pos].

    @param pos default = 0
    @param len default = [Bytes.length str - pos] *)

val to_string : ?pos : int -> ?len : int -> t -> string
(** [to_string ?pos ?len bstr] @return a new string that is equivalent
    to the substring of length [len] in [bstr] starting at position [pos].

    @param pos default = 0
    @param len default = [length bstr - pos]

    @raise Invalid_argument if the string would exceed runtime limits. *)

val to_bytes : ?pos : int -> ?len : int -> t -> bytes
(** [to_bytes ?pos ?len bstr] @return a new byte sequence that is equivalent
    to the substring of length [len] in [bstr] starting at position [pos].

    @param pos default = 0
    @param len default = [length bstr - pos]

    @raise Invalid_argument if the bytes would exceed runtime limits. *)

val concat : ?sep:t -> t list -> t
(** [concat ?sep list] returns the concatenation of [list] with [sep] in between each. *)

(** {2 Checking} *)

val check_args : loc : string -> pos : int -> len : int -> t -> unit
(** [check_args ~loc ~pos ~len bstr] checks the position and length
    arguments [pos] and [len] for bigstrings [bstr].  @raise
    Invalid_argument if these arguments are illegal for the given
    bigstring using [loc] to indicate the calling context. *)

val get_opt_len : t -> pos : int -> int option -> int
(** [get_opt_len bstr ~pos opt_len] @return the length of a subbigstring
    in [bstr] starting at position [pos] and given optional length
    [opt_len].  This function does not check the validity of its
    arguments.  Use {!check_args} for that purpose. *)


(** {2 Accessors} *)

val length : t -> int
(** [length bstr] @return the length of bigstring [bstr]. *)

external unsafe_destroy_and_resize : t -> len : int -> t = "bigstring_realloc"
(** [unsafe_destroy_and_resize bstr ~len] reallocates the memory backing
    [bstr] and returns a new bigstring that starts at position 0 and has
    length [len]. If [len] is greater than [length bstr] then the newly
    allocated memory will not be initialized.

    Similar to [unsafe_destroy], this operation is safe unless you have passed
    the bigstring to another thread that is performing operations on it at the
    same time.  Access to [bstr] after this operation will yield array bounds
    exceptions.

    @raise Failure if the bigstring has already been deallocated (or deemed
    "external", which is treated equivalently), if it is backed by a memory
    map, or if it has proxies, i.e. other bigstrings referring to the same
    data. *)

val sub_shared : ?pos : int -> ?len : int -> t -> t
(** [sub_shared ?pos ?len bstr] @return the sub-bigstring in [bstr]
    that starts at position [pos] and has length [len].  The sub-bigstring
    shares the same memory region, i.e. modifying it will modify the
    original bigstring.  Holding on to the sub-bigstring will also keep
    the (usually bigger) original one around.

    @param pos default = 0
    @param len default = [Bigstring.length bstr - pos] *)

(** [get t pos] returns the character at [pos] *)
external get : t -> int -> char = "%caml_ba_ref_1"

(** [set t pos] sets the character at [pos] *)
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" [@@noalloc]
(** [is_mmapped bstr] @return whether the bigstring [bstr] is
    memory-mapped. *)

(** {2 Blitting} *)

(** [blit ~src ?src_pos ?src_len ~dst ?dst_pos ()] blits [src_len] characters
    from [src] starting at position [src_pos] to [dst] at position [dst_pos].

    @raise Invalid_argument if the designated ranges are out of bounds. *)

include Blit.S with type t := t

module To_string : sig
  val blit : (t, bytes) Blit.blit
  [@@deprecated "[since 2017-10] use [Bigstring.To_bytes.blit] instead"]
  val blito : (t, bytes) Blit.blito
  [@@deprecated "[since 2017-10] use [Bigstring.To_bytes.blito] instead"]
  val unsafe_blit : (t, bytes) Blit.blit
  [@@deprecated "[since 2017-10] use [Bigstring.To_bytes.unsafe_blit] instead"]
  val sub : (t, string) Blit.sub
  val subo : (t, string) Blit.subo
end
module From_string : Blit.S_distinct with type src := string with type dst := t

module To_bytes   : Blit.S_distinct with type src := t     with type dst := bytes
module From_bytes : Blit.S_distinct with type src := bytes with type dst := t

(** {2 Reading/writing bin-prot} *)

(** These functions write the "size-prefixed" bin-prot format that is used by, e.g.,
    async's [Writer.write_bin_prot], [Reader.read_bin_prot] and
    [Unpack_buffer.Unpack_one.create_bin_prot]. *)

(** [write_bin_prot t writer a] writes [a] to [t] starting at [pos], and returns the index
    in [t] immediately after the last byte written.  It raises if [pos < 0] or if [a]
    doesn't fit in [t]. *)
val write_bin_prot
  :  t
  -> ?pos:int  (** default is 0 *)
  -> 'a Bin_prot.Type_class.writer
  -> 'a
  -> int

(** The [read_bin_prot*] functions read from the region of [t] starting at [pos] of length
    [len].  They return the index in [t] immediately after the last byte read.  They raise
    if [pos] and [len] don't describe a region of [t]. *)
val read_bin_prot
  : t -> ?pos:int -> ?len:int -> 'a Bin_prot.Type_class.reader -> ('a * int) Or_error.t
val read_bin_prot_verbose_errors
  : t
  -> ?pos:int
  -> ?len:int
  -> 'a Bin_prot.Type_class.reader
  -> [ `Invalid_data of Error.t
     | `Not_enough_data
     | `Ok of ('a * int)
     ]

(** {2 Search} *)

(** [find ?pos ?len char t] returns [Some i] for the smallest [i >= pos] such that
    [t.{i} = char], or [None] if there is no such [i].

    @param pos default = 0
    @param len default = [length bstr - pos] *)
val find
  :  ?pos : int
  -> ?len : int
  -> char
  -> t
  -> int option

(** Same as [find], but does no bounds checking, and returns a negative value instead of
    [None] if [char] is not found. *)
external unsafe_find : t -> char -> pos:int -> len:int -> int = "bigstring_find" [@@noalloc]


(** {2 Destruction} *)

(** [unsafe_destroy bstr] destroys the bigstring by deallocating its associated data or,
    if memory-mapped, unmapping the corresponding file, and setting all dimensions to
    zero.  This effectively frees the associated memory or address-space resources
    instantaneously.  This feature helps working around a bug in the current OCaml
    runtime, which does not correctly estimate how aggressively to reclaim such resources.

    This operation is safe unless you have passed the bigstring to another thread that is
    performing operations on it at the same time.  Access to the bigstring after this
    operation will yield array bounds exceptions.

    @raise Failure if the bigstring has already been deallocated (or deemed "external",
    which is treated equivalently), or if it has proxies, i.e. other bigstrings referring
    to the same data. *)
external unsafe_destroy : t -> unit = "bigstring_destroy_stub"

(** {2 Accessors for parsing binary values, analogous to binary_packing}

    These are in Bigstring rather than a separate module because:

    1) Existing binary_packing requires copies and does not work with bigstrings
    2) The accessors rely on the implementation of bigstring, and hence should
    change should the implementation of bigstring move away from Bigarray.
    3) Bigstring already has some external C functions, so it didn't require many
    changes to the OMakefile ^_^.

    In a departure from Binary_packing, the naming conventions are chosen to be close to
    C99 stdint types, as it's a more standard description and it is somewhat useful in
    making compact macros for the implementations.  The accessor names contain endian-ness
    to allow for branch-free implementations

    <accessor>  ::= <unsafe><operation><type><endian>
    <unsafe>    ::= unsafe_ | ''
    <operation> ::= get_ | set_
    <type>      ::= int8 | uint8 | int16 | uint16 | int32 | uint32 | int64 | uint64
    <endian>    ::= _le | _be | ''

    The "unsafe_" prefix indicates that these functions do no bounds checking. *)

val get_int8                : t -> pos:int -> int
val set_int8                : t -> pos:int -> int -> unit
val get_uint8               : t -> pos:int -> int
val set_uint8               : t -> pos:int -> int -> unit

val unsafe_get_int8         : t -> pos:int -> int
val unsafe_set_int8         : t -> pos:int -> int -> unit
val unsafe_get_uint8        : t -> pos:int -> int
val unsafe_set_uint8        : t -> pos:int -> int -> unit

(** {2 16-bit methods} *)

val get_int16_le            : t -> pos:int -> int
val get_int16_be            : t -> pos:int -> int
val set_int16_le            : t -> pos:int -> int -> unit
val set_int16_be            : t -> pos:int -> int -> unit

val unsafe_get_int16_le     : t -> pos:int -> int
val unsafe_get_int16_be     : t -> pos:int -> int
val unsafe_set_int16_le     : t -> pos:int -> int -> unit
val unsafe_set_int16_be     : t -> pos:int -> int -> unit

val get_uint16_le    : t -> pos:int -> int
val get_uint16_be    : t -> pos:int -> int
val set_uint16_le    : t -> pos:int -> int -> unit
val set_uint16_be    : t -> pos:int -> int -> unit

val unsafe_get_uint16_le    : t -> pos:int -> int
val unsafe_get_uint16_be    : t -> pos:int -> int
val unsafe_set_uint16_le    : t -> pos:int -> int -> unit
val unsafe_set_uint16_be    : t -> pos:int -> int -> unit

(** {2 32-bit methods} *)

val get_int32_le     : t -> pos:int -> int
val get_int32_be     : t -> pos:int -> int
val set_int32_le     : t -> pos:int -> int -> unit
val set_int32_be     : t -> pos:int -> int -> unit

val unsafe_get_int32_le     : t -> pos:int -> int
val unsafe_get_int32_be     : t -> pos:int -> int
val unsafe_set_int32_le     : t -> pos:int -> int -> unit
val unsafe_set_int32_be     : t -> pos:int -> int -> unit

val get_uint32_le    : t -> pos:int -> int
val get_uint32_be    : t -> pos:int -> int
val set_uint32_le    : t -> pos:int -> int -> unit
val set_uint32_be    : t -> pos:int -> int -> unit

val unsafe_get_uint32_le    : t -> pos:int -> int
val unsafe_get_uint32_be    : t -> pos:int -> int
val unsafe_set_uint32_le    : t -> pos:int -> int -> unit
val unsafe_set_uint32_be    : t -> pos:int -> int -> unit

(** Similar to the usage in binary_packing, the below methods are treating the value being
    read (or written), as an ocaml immediate integer, as such it is actually 63 bits. If
    the user is confident that the range of values used in practice will not require
    64-bit precision (i.e. Less than Max_Long), then we can avoid allocation and use an
    immediate.  If the user is wrong, an exception will be thrown (for get). *)

(** {2 64-bit signed values} *)

val get_int64_le_exn   : t -> pos:int -> int
val get_int64_be_exn   : t -> pos:int -> int
val get_int64_le_trunc : t -> pos:int -> int
val get_int64_be_trunc : t -> pos:int -> int
val set_int64_le       : t -> pos:int -> int -> unit
val set_int64_be       : t -> pos:int -> int -> unit

val unsafe_get_int64_le_exn   : t -> pos:int -> int
val unsafe_get_int64_be_exn   : t -> pos:int -> int
val unsafe_get_int64_le_trunc : t -> pos:int -> int
val unsafe_get_int64_be_trunc : t -> pos:int -> int
val unsafe_set_int64_le       : t -> pos:int -> int -> unit
val unsafe_set_int64_be       : t -> pos:int -> int -> unit

(** {2 64-bit unsigned values} *)

val get_uint64_be_exn : t -> pos:int -> int
val get_uint64_le_exn : t -> pos:int -> int
val set_uint64_le     : t -> pos:int -> int -> unit
val set_uint64_be     : t -> pos:int -> int -> unit

val unsafe_get_uint64_be_exn : t -> pos:int -> int
val unsafe_get_uint64_le_exn : t -> pos:int -> int
val unsafe_set_uint64_le     : t -> pos:int -> int -> unit
val unsafe_set_uint64_be     : t -> pos:int -> int -> unit

(** {2 32-bit methods with full precision} *)

val get_int32_t_le : t -> pos:int -> Int32.t
val get_int32_t_be : t -> pos:int -> Int32.t
val set_int32_t_le : t -> pos:int -> Int32.t -> unit
val set_int32_t_be : t -> pos:int -> Int32.t -> unit

val unsafe_get_int32_t_le : t -> pos:int -> Int32.t
val unsafe_get_int32_t_be : t -> pos:int -> Int32.t
val unsafe_set_int32_t_le : t -> pos:int -> Int32.t -> unit
val unsafe_set_int32_t_be : t -> pos:int -> Int32.t -> unit

(** {2 64-bit methods with full precision} *)

val get_int64_t_le : t -> pos:int -> Int64.t
val get_int64_t_be : t -> pos:int -> Int64.t
val set_int64_t_le : t -> pos:int -> Int64.t -> unit
val set_int64_t_be : t -> pos:int -> Int64.t -> unit

val unsafe_get_int64_t_le : t -> pos:int -> Int64.t
val unsafe_get_int64_t_be : t -> pos:int -> Int64.t
val unsafe_set_int64_t_le : t -> pos:int -> Int64.t -> unit
val unsafe_set_int64_t_be : t -> pos:int -> Int64.t -> unit

(** Similar to [Binary_packing.unpack_tail_padded_fixed_string] and
    [.pack_tail_padded_fixed_string]. *)
val get_tail_padded_fixed_string : padding:char -> t -> pos:int -> len:int -> unit -> string
val set_tail_padded_fixed_string : padding:char -> t -> pos:int -> len:int -> string -> unit

val get_head_padded_fixed_string : padding:char -> t -> pos:int -> len:int -> unit -> string
val set_head_padded_fixed_string : padding:char -> t -> pos:int -> len:int -> string -> unit


