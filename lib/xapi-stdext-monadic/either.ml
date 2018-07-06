type ('a,'b) t = Left of 'a | Right of 'b

module Monad = Monad.M2.Make (struct

    type ('a, 'b) m = ('b, 'a) t

    let bind value f =
      match value with
      | Left value -> Left value
      | Right value -> f value

    let return value = Right value

  end)

let left x = Left x
let right x = Right x
let is_left = function
  | Left _ -> true
  | Right _ -> false
let is_right x = not (is_left x)
let to_option = function
  | Right x -> Some x
  | Left _ -> None

let cat_right l =
  let unbox_list a = List.map Opt.unbox (List.filter Opt.is_boxed a) in
  unbox_list (List.map to_option l)

let join = function
  | Right (Right x) -> Right x
  | Left x -> Left (Left x)
  | Right (Left x) -> Left (Right x)

let swap = function
  | Right x -> Left x
  | Left x -> Right x

let of_exception f =
  try Right (f ())
  with e -> Left e
