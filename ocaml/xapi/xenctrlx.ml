
module D = Debug.Make(struct let name = "xenctrlx" end)

let handle = ref None

let with_intf f =
  match !handle with
    | Some xc -> f xc
    | None ->
      let xc =
        try
          Xenctrl.interface_open ()
        with
        | e ->
            let msg = Printexc.to_string e in
            failwith @@ "failed to open xenctrl interface: "^msg
      in
        handle := Some (xc);
        f xc
 
let close () =
  match !handle with
  | Some xc -> Xenctrl.interface_close xc
  | None    -> ()

let () = at_exit close

