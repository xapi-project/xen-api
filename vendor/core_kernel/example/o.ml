open Core_kernel

let x =
  Float.O.((3. + 4.) / of_int 2)


let () =
  if Float.O.(sqrt 3. > 4.) then printf "yo%!" else printf "gabba%!"
