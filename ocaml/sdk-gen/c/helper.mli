val formatted_wrap : Format.formatter -> string -> unit
(** Recursively formats the input string
    based on spaces and newlines, ensuring proper indentation.

    @param formatter The formatter to output the formatted string.
    @param s The input string to be formatted. *)

val comment : bool -> ?indent:int -> string -> string
(** Formats a comment block with optional indentation.
    @param indent Optional indentation level.
    @param doc Include an extra '*' for documentation.
    @param s String content of the comment.
    @return Formatted comment block. *)
