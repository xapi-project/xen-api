(** Utility functions for parsing and outputing strings containing known numbers
    of digits.  Used primarily for building functions for reading in and writing
    out Time related values. *)

open! Import


(** {2 Write digit functions}

    [write_int63 bytes ~pos ~digits int63] writes the string representation of [int63],
    0-padded to fill [digits] characters, into [bytes] starting at position [pos]. Raises
    if [int] is negative or is too long for [bytes], if [pos] is an invalid index in
    [bytes] for the number of digits, or if [digits < 1]. *)
val write_int63 : bytes -> pos:int -> digits:int -> Int63.t -> unit

(** [write_*_digit_int] is like [write_int63] for a hard-coded number of digits and for
    [int] rather than [Int63.t]. *)
val write_1_digit_int : bytes -> pos:int -> int -> unit
val write_2_digit_int : bytes -> pos:int -> int -> unit
val write_3_digit_int : bytes -> pos:int -> int -> unit
val write_4_digit_int : bytes -> pos:int -> int -> unit
val write_5_digit_int : bytes -> pos:int -> int -> unit
val write_6_digit_int : bytes -> pos:int -> int -> unit
val write_7_digit_int : bytes -> pos:int -> int -> unit
val write_8_digit_int : bytes -> pos:int -> int -> unit
val write_9_digit_int : bytes -> pos:int -> int -> unit

(** {2 Read digit functions}

    [read_int63 string ~pos ~digits] parses [digits] characters starting at [pos] in
    [string] and returns the corresponding [Int63.t]. It raises if [digits < 1] or
    [pos < 0] or [pos + digits > String.length string]. *)
val read_int63 : string -> pos:int -> digits:int -> Int63.t

(** [read_*_digit_int] is like [read_int63] for a hard-coded number of digits and for
    [int] rather than [Int63.t]. *)
val read_1_digit_int : string -> pos:int -> int
val read_2_digit_int : string -> pos:int -> int
val read_3_digit_int : string -> pos:int -> int
val read_4_digit_int : string -> pos:int -> int
val read_5_digit_int : string -> pos:int -> int
val read_6_digit_int : string -> pos:int -> int
val read_7_digit_int : string -> pos:int -> int
val read_8_digit_int : string -> pos:int -> int
val read_9_digit_int : string -> pos:int -> int
