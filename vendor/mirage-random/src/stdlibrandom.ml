
type buffer = Cstruct.t

type g = unit

let generate ?g:_g n =
  let b = Cstruct.create n in
  for i = 0 to pred n do
    Cstruct.set_uint8 b i (Random.int 256)
  done ;
  b

let initialize () =
  Random.self_init ()
