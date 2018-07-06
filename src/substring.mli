(** A substring is a contiguous set of characters within a string. Creating a substring
    does not copy. Therefore modifying the string also modifies the substring. *)

include Make_substring.S with type base = bytes (** @inline *)
