let finally f g =
            try
              let result = f () in
              g ();
              result
            with e ->
              g ();
              raise e

let with_output_file filename f =
  let oc = open_out filename in
finally (fun () -> f oc) (fun () -> close_out oc)

let with_input_file filename f =
  let ic = open_in filename in
finally (fun () -> f ic) (fun () -> close_in ic)

let dd ic oc =
  try
    while true do
      output_char oc (input_char ic)
    done
  with End_of_file -> ()

let print_file_to oc filename =
  with_input_file filename
    (fun ic ->
       dd ic oc
    )
