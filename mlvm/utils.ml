let (--) = Int64.sub
let (++) = Int64.add
let (//) = Int64.div
let ( *** ) = Int64.mul

let int64_round_up value blocksize =
  blocksize *** ((value ++ blocksize -- 1L) // blocksize)
