open Pervasiveext
open Listext

type ('a,'b) t = Left of 'a | Right of 'b

let left x = Left x
let right x = Right x
let is_left = function
	| Left _ -> true
	| Right _ -> false
let is_right x = not ++ is_left $ x
let to_option = function
	| Right x -> Some x
	| Left _ -> None

let cat_right l = List.unbox_list ++ List.map to_option $ l

let join = function
	| Right (Right x) -> Right x
	| Left x -> Left (Left x)
	| Right (Left x) -> Left (Right x)

let swap = function
	| Right x -> Left x
	| Left x -> Right x
