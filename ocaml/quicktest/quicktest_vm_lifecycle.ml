type api_call = Shutdown | Reboot [@@deriving rpcty]

type api_mode = Clean | Hard [@@deriving rpcty]

type api = api_mode * api_call [@@deriving rpcty]

type internal_op = Internal_reboot | Internal_halt | Internal_crash
[@@deriving rpcty]

type result = Rebooted | Halted [@@deriving rpcty]

type test = Api of api | Internal_op of internal_op [@@deriving rpcty]

let expected_result = function
  | Api (_, Shutdown) ->
      Halted
  | Api (_, Reboot) ->
      Rebooted
  | Internal_op (Internal_halt | Internal_crash) ->
      Halted
  | Internal_op Internal_reboot ->
      Rebooted

let string_of_test test =
  let string_of ty x = Rpcmarshal.marshal ty x |> Rpc.to_string in
  Printf.sprintf "%s -> %s"
    (string_of typ_of_test test)
    (string_of typ_of_result (expected_result test))

let all_possible_tests =
  (* We omit clean shutdown & reboot because the VM not be PV and will lack
     these features *)
  [
    Api (Hard, Shutdown)
  ; Api (Hard, Reboot)
  ; Internal_op Internal_reboot
  ; Internal_op Internal_halt
  ; Internal_op Internal_crash
  ]

let one rpc session_id vm test =
  print_endline ("Running test " ^ string_of_test test) ;
  if Client.Client.VM.get_power_state ~rpc ~session_id ~self:vm = `Halted then
    Client.Client.VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false ;
  (* wait for the guest to actually start up *)
  Thread.delay 15. ;
  let call_api = function
    | Clean, Shutdown ->
        Client.Client.VM.clean_shutdown ~rpc ~session_id ~vm
    | Hard, Shutdown ->
        Client.Client.VM.hard_shutdown ~rpc ~session_id ~vm
    | Clean, Reboot ->
        Client.Client.VM.clean_reboot ~rpc ~session_id ~vm
    | Hard, Reboot ->
        Client.Client.VM.hard_reboot ~rpc ~session_id ~vm
  in
  let domid = Client.Client.VM.get_domid ~rpc ~session_id ~self:vm in
  ( match test with
  | Internal_op internal_op -> (
      (* The Xenctrl module is used in xenopsd *)
      let reason =
        match internal_op with
        | Internal_reboot ->
            Xenctrl.Reboot
        | Internal_halt ->
            Xenctrl.Poweroff
        | Internal_crash ->
            Xenctrl.Crash
      in
      try
        Xenctrl.with_intf (fun xc ->
            Xenctrl.domain_shutdown xc (Int64.to_int domid) reason
        )
      with e -> Printf.printf "Ignoring exception: %s" (Printexc.to_string e)
    )
  | Api api ->
      call_api api
  ) ;
  let wait_for_domid p =
    let start = Unix.gettimeofday () in
    let finished = ref false in
    while Unix.gettimeofday () -. start < 300. && not !finished do
      finished := p (Client.Client.VM.get_domid ~rpc ~session_id ~self:vm) ;
      if not !finished then Thread.delay 1.
    done ;
    if not !finished then failwith "timeout"
  in
  match expected_result test with
  | Rebooted ->
      wait_for_domid (fun domid' -> domid <> domid')
  | Halted ->
      wait_for_domid (fun domid' -> domid' = -1L)

let test rpc session_id sr_info vm_template () =
  let sr = sr_info.Qt.sr in
  Qt.VM.with_new rpc session_id ~template:vm_template ~sr (fun vm ->
      List.iter (one rpc session_id vm) all_possible_tests
  )

let tests () =
  let open Qt_filter in
  [
    [("VM lifecycle tests", `Slow, test)]
    |> conn
    |> sr SR.(all |> allowed_operations [`vdi_create])
    |> vm_template "CoreOS"
  ]
  |> List.concat
