external _get_major_minor : string -> int * int * int = "stub_statdev_get_major_minor"

let get_major_minor path = 
  let errno, major, minor = _get_major_minor path in
  if errno <> 0 then failwith (Printf.sprintf "Cannot stat path: %s (errno = %d)" path errno);
  major, minor
