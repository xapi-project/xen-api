
let debug_hook : (('b, unit, string, unit) format4 -> 'b) option ref = ref None
let debug string = match !debug_hook with Some x -> x "%s" string | None -> Printf.fprintf stderr "%s\n" string
