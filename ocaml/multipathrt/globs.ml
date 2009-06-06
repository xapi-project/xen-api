
(* Global values *)

let meg = Int64.mul 1024L 1024L
let two_megs = Int64.mul meg 2L
let four_megs = Int64.mul meg 4L
let four_gigs = Int64.mul four_megs 1024L
let eight_megs = Int64.mul meg 8L
let sixteen_megs = Int64.mul meg 16L
let thirtytwo_megs = Int64.mul meg 32L

let binary_name = "multipathrt"
let helper_plugin = "multipathrt-helper"

let hostname = ref ""
let username = ref "root"
let password = ref ""
let tc = ref 0


