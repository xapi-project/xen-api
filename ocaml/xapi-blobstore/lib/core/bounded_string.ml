module Make (C : sig
  val max_length : int
end) =
struct
  type t = string

  let max_length = C.max_length

  let of_string_exn s =
    let n = String.length s in
    if n <= max_length then
      s
    else
      Fmt.invalid_arg "String too long: %d > %d" n max_length

  let to_string s = s

  let compare = String.compare
end
