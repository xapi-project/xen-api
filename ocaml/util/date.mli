type iso8601
val of_float : float -> iso8601
val to_float : iso8601 -> float
val to_string : iso8601 -> string
val of_string : string -> iso8601
val never: iso8601

type rfc822
val rfc822_of_float : float -> rfc822
val rfc822_to_string : rfc822 -> string
