open! Import
open! Core_kernel.String

(*TEST = slice "hey" 0 0 = ""*) (* This is what I would expect *)
let%test _ = slice "hey" 0 0 = "hey" (* But this is what we get! *)

let%test _ = slice "hey" 0 1 = "h"
let%test _ = slice "hey" 0 2 = "he"
let%test _ = slice "hey" 0 3 = "hey"
let%test _ = slice "hey" 1 1 = ""
let%test _ = slice "hey" 1 2 = "e"
let%test _ = slice "hey" 1 3 = "ey"
let%test _ = slice "hey" 2 2 = ""
let%test _ = slice "hey" 2 3 = "y"
let%test _ = slice "hey" 3 3 = ""
