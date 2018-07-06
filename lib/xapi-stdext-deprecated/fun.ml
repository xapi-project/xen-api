(* just forgets it's second argument: *)
let const a _ = a

let uncurry f (a,b) = f a b

let id a = a

let flip f a b = f b a

let on op f x y = op (f x) (f y)

let comp f g x = f (g x)
let (++) f g x = comp f g x

let comp2 f g a b = f (g a b)
let (+++) f g a b = comp2 f g a b

let ($) f a = f a
