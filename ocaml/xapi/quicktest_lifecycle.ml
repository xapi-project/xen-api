

type 'a api_call =
  | Shutdown of 'a
  | Reboot of 'a

type api_mode =
  | Clean
  | Hard

type api = api_mode api_call

type parallel_op =
  | Internal_reboot
  | Internal_halt
  | Internal_suspend
  | Internal_crash

type code_path =
  | Sync
  | Event
  | Both

type result =
  | Rebooted
  | Halted

let final_guest_state = function
  | Shutdown _ -> Halted
  | Reboot _ -> Rebooted

type test = {
  api: api option;
  parallel_op: parallel_op option;
  code_path: code_path;
}

let string_of_result = function
  | Rebooted -> "Reboot"
  | Halted -> "Halt"

let expected_result = function
  | { api = Some (Shutdown _); parallel_op = Some _; code_path = (Sync|Both) } -> Some Halted
  | { api = Some (Reboot _);   parallel_op = Some _; code_path = (Sync|Both) } -> Some Rebooted
  | { api = Some (Shutdown _); parallel_op = None;   code_path = (Sync|Event|Both) } -> Some Halted
  | { api = Some (Reboot _);   parallel_op = None;   code_path = (Sync|Event|Both) } -> Some Rebooted
  | { parallel_op = Some (Internal_halt | Internal_crash); code_path = Event } -> Some Halted
  | { parallel_op = Some Internal_reboot; code_path = Event } -> Some Rebooted

  | _ -> None (* invalid test *)


let string_of_test x =
  let string_of_api = function
    | Shutdown Clean   -> "clean_shutdown"
    | Shutdown Hard    -> "hard_shutdown "
    | Reboot Clean     -> "clean_reboot  "
    | Reboot Hard      -> "hard_reboot   " in
  let string_of_parallel_op = function
    | Internal_reboot  -> "reboot        "
    | Internal_halt    -> "halt          "
    | Internal_suspend -> "suspend       "
    | Internal_crash   -> "crash         " in
  let string_of_code_path = function
    | Sync             -> "synch         "
    | Event            -> "event         "
    | Both             -> "both          " in
  let dm f x = match x with
    | None             -> "Nothing       "
    | Some x           -> f x in
  Printf.sprintf "%s %s %s -> %s"
    (dm string_of_api x.api) (dm string_of_parallel_op x.parallel_op) (string_of_code_path x.code_path)
    (match expected_result x with None -> "invalid" | Some y -> string_of_result y)
open List

let all_possible_tests =
  let all_api_variants x =
    [ { x with api = None };
      { x with api = Some (Shutdown Clean) };
      { x with api = Some (Shutdown Hard) };
      { x with api = Some (Reboot Clean) };
      { x with api = Some (Reboot Hard) } ] in
  let all_parallel_op_variants x =
    [ { x with parallel_op = None };
      { x with parallel_op = Some Internal_reboot };
      { x with parallel_op = Some Internal_halt };
      { x with parallel_op = Some Internal_suspend };
      { x with parallel_op = Some Internal_crash } ] in
  let all_code_path_variants x =
    [ { x with code_path = Sync };
      { x with code_path = Event };
      { x with code_path = Both } ] in

  let xs = [ { api = None; parallel_op = None; code_path = Sync } ] in
  concat (map all_code_path_variants (concat (map all_parallel_op_variants (concat (map all_api_variants xs)))))

let all_valid_tests = List.filter (fun t -> expected_result t <> None) all_possible_tests

   (*
let _ =
  List.iter print_endline (map string_of_test all_valid_tests);
  Printf.printf "In total there are %d tests.\n" (List.length all_valid_tests)
	  *)

open Quicktest_common
open Client

let one s vm test =
  let open Stdext in
  let t = make_test (string_of_test test) 1 in
  start t;
  let event = "/tmp/fist_disable_event_lifecycle_path" in
  let sync = "/tmp/fist_disable_sync_lifecycle_path" in
  let simulate = "/tmp/fist_simulate_internal_shutdown" in
  let delay = "/tmp/fist_disable_reboot_delay" in

  Pervasiveext.finally
    (fun () ->
       try
         begin
           Unixext.unlink_safe simulate;
           Unixext.touch_file delay;
           match test.code_path with
           | Sync ->
             Unixext.unlink_safe sync;
             Unixext.touch_file event
           | Event ->
             Unixext.unlink_safe event;
             Unixext.touch_file sync
           | Both ->
             Unixext.unlink_safe sync;
             Unixext.unlink_safe event
         end;
         if Client.VM.get_power_state !rpc s vm = `Halted
         then Client.VM.start !rpc s vm false false;
         (* wait for the guest to actually start up *)
         Thread.delay 15.;

         let call_api = function
           | Shutdown Clean -> Client.VM.clean_shutdown !rpc s vm
           | Shutdown Hard -> Client.VM.hard_shutdown !rpc s vm
           | Reboot Clean -> Client.VM.clean_reboot !rpc s vm
           | Reboot Hard -> Client.VM.hard_reboot !rpc s vm in

         let domid = Client.VM.get_domid !rpc s vm in
         begin match test with
           | { api = None; parallel_op = Some x } ->
             let reason = match x with
               | Internal_reboot -> Xenctrl.Reboot
               | Internal_halt -> Xenctrl.Poweroff
               | Internal_crash -> Xenctrl.Crash
               | Internal_suspend -> Xenctrl.Suspend in
             begin
               try
                 Xenctrl.with_intf (fun xc -> Xenctrl.domain_shutdown xc (Int64.to_int domid) reason)
               with e ->
                 debug t (Printf.sprintf "Ignoring exception: %s" (Printexc.to_string e))
             end
           | { api = Some x; parallel_op = Some y } ->
             let reason = match y with
               | Internal_reboot -> "reboot"
               | Internal_halt -> "halt"
               | Internal_crash -> "crash"
               | Internal_suspend -> "suspend" in
             Unixext.write_string_to_file simulate reason;
             call_api x
           | { api = Some x; parallel_op = None } ->
             call_api x
           | t -> failwith (Printf.sprintf "Invalid test: %s" (string_of_test t))
         end;

         let wait_for_domid p =
           let start = Unix.gettimeofday () in
           let finished = ref false in
           while Unix.gettimeofday () -. start < 300. && (not !finished) do
             finished := p (Client.VM.get_domid !rpc s vm);
             if not !finished then Thread.delay 1.
           done;
           if not !finished then failwith "timeout"
         in

         begin match expected_result test with
           | None -> failwith (Printf.sprintf "Invalid test: %s" (string_of_test test))
           | Some Rebooted ->
             wait_for_domid (fun domid' -> domid <> domid')
           | Some Halted ->
             wait_for_domid (fun domid' -> domid' = -1L)
         end
       with e -> failed t (Printexc.to_string e)
    )
    (fun () ->
       Unixext.unlink_safe sync;
       Unixext.unlink_safe event;
       Unixext.unlink_safe delay
    );
  success t

let test s vm =
  List.iter (one s vm) all_valid_tests
