
let default_user = "unixpwd"

let cycle user rounds n =
  let pw = Printf.sprintf "unixpwd-%06d" n in
  Unixpwd.setspw user pw;
  ignore (Unixpwd.unshadow () |> String.length);
  assert (Unixpwd.getspw user = pw);
  Unixpwd.setpwd user pw;
  assert (Unixpwd.getpwd user = pw);
  if n mod (rounds/80) = 0 then begin
    print_char '='; flush stdout
  end

let main () =
  let rounds = 500_000 in
  let user = match Sys.argv with
    | [|_;name|] -> name
    | _          -> default_user in
  for n = 1 to rounds do
    cycle user rounds n
  done;
  print_char '\n'

let () =
  if !Sys.interactive then
    ()
  else
    try
      main ()
    with
      e ->
      Printf.eprintf "error: %s\n" (Printexc.to_string e);
      exit 1

