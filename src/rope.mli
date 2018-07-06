(** A rope is a standard data structure that represents a single string as a tree of
    strings, allowing concatenation to do no work up front.

    That is, a string formed by many [Rope] concatenations followed by a [to_string] needs
    only copy each input to the output once, whereas a string expression looking like [a ^
    b ^ c ^ ... ^ z] must create an intermediate string for every concatenation, and will
    copy the original data into and out of short-lived temporary strings many times.

    On the other hand, because [String.concat [ s1; s2; s3; ... ]] allocates a single
    string and copies the inputs into it, [Rope] is no improvement over that usage.
    [Rope] becomes useful when the construction of the sequence of strings is more
    complex -- a good example is prettyprinting an expression language, where you need to
    parenthesize subexpressions (appending a short string at both ends) and handle infix
    binary operators (appending two long strings both made up of many parts, with a short
    string in between).

    Any operations that would produce a [Rope] longer than [String.max_length] raise
    instead. They are not marked with [_exn] on their names since (at least on 64-bit)
    this number is far in excess of the size of your memory, so isn't likely to come up in
    practice.

    A more fully-featured Rope implementation is available in the zed library.
*)

open! Import

type t

(** Takes O(1) time. The string isn't copied, so don't mutate it. *)
val of_string : string -> t

val empty : t

val is_empty : t -> bool
val length   : t -> int

(** Allocates a fresh string, so takes time proportional to the total
    size of the result. *)
val to_string : t -> string

(** These take time proportional to the number of [t]'s passed. *)
val ( ^ )        :      t -> t       -> t
val concat       : ?sep:t -> t list  -> t
val concat_array : ?sep:t -> t array -> t

(** Appends the contents of the Rope at the end of a destination buffer. *)
val add_to_buffer : t -> Buffer.t -> unit
