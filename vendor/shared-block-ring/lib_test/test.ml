(*
 * Copyright (C) 2013-2015 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Result
open Lwt
open OUnit

(* Let's try to adopt the conventions of Rresult.R *)
let get_ok = function | Ok x -> x | Error _ -> raise (Invalid_argument "get_ok encountered an Error")
let get_error = function | Error x -> x | Ok _ -> raise (Invalid_argument "get_error encountered an OK")

module Lwt_result = struct
  let (>>=) m f = m >>= fun x -> f (get_ok x)
end

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns d =
    assert (d >= 0L); (* Can't sleep for -ve time *)
    Duration.to_f d |> Lwt_unix.sleep
end

let find_unused_file () =
  (* Find a filename which doesn't exist *)
  let rec does_not_exist i =
    let name = Printf.sprintf "%s/mirage-block-test.%d.%d"
      (Filename.get_temp_dir_name ()) (Unix.getpid ()) i in
    if Sys.file_exists name
    then does_not_exist (i + 1)
    else name in
  does_not_exist 0

let alloc sector_size =
  let page = Io_page.(to_cstruct (get 1)) in
  let sector = Cstruct.sub page 0 sector_size in
  sector

let fill_with_message buffer message =
  for i = 0 to Cstruct.len buffer - 1 do
    Cstruct.set_char buffer i (message.[i mod (String.length message)])
  done

let fresh_file nbytes =
  let name = find_unused_file () in
  Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY; Lwt_unix.O_TRUNC ] 0o0666 >>= fun fd ->
  (* Write the last sector to make sure the file has the intended size *)
  Lwt_unix.LargeFile.lseek fd Int64.(sub nbytes 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
  let sector = alloc 512 in
  fill_with_message sector "\xde\xead\xbe\xef";
  Block.really_write fd sector >>= fun () ->
  return name

let interesting_lengths = [
  0; (* possible base case *)
  1; (* easily fits inside a sector with the 4 byte header *)
  512 - 4 - 1;
  512 - 4;
  512 - 4 + 1;
  2000; (* More than 3 sectors *)
]

(* number of pushes before we pop *)
let interesting_batch_sizes = [
  1;
  2;
  3;
  4;
  5;
]

module Op = struct
  type t = string
  let to_cstruct x =
    let c = Cstruct.create (String.length x) in
    Cstruct.blit_from_string x 0 c 0 (String.length x);
    c
  let of_cstruct x = Some (Cstruct.to_string x)
end
module Int = struct
  type t = int32
  let to_cstruct x =
    let c = Cstruct.create 4 in
    Cstruct.LE.set_uint32 c 0 x;
    c
  let of_cstruct x = Some (Cstruct.LE.get_uint32 x 0)
end

module R = Shared_block.Ring.Make(Log)(Block)(Op)
open R

let size = 16384L

let test_push_pop _length batch () =
  let t : unit Lwt.t =
    fresh_file size
    >>= fun name ->

    let payload = "All work and no play makes Dave a dull boy.\n" in
    let toobig = Bytes.create Int64.(to_int size) in
    Block.connect name >>= fun disk ->
    let open Lwt_result in
    Producer.create ~disk () >>= fun () ->
    Producer.attach ~client:"test" ~queue:"test_push_pop" ~disk () >>= fun producer ->
    Consumer.attach ~client:"test" ~queue:"test_push_pop" ~disk () >>= fun consumer ->
    (* check debug_info works *)
    let _ = Producer.debug_info producer in
    let _ = Consumer.debug_info consumer in

    let finished = ref false in
    let rec spin () =
      if !finished then return () else begin
        Producer.state producer >>= fun _ ->
        spin ()
      end in
    let _ = spin () in

    let rec loop = function
      | 0 -> return ()
      | n ->
        let open Lwt in
        Consumer.pop ~t:consumer () >>= fun r ->
        assert_equal `Retry (get_error r);
        let rec push = function
        | 0 -> return ()
        | m ->
          Producer.push ~t:producer ~item:(Bytes.to_string toobig) () >>= fun r ->
          ignore(get_error r);
          Producer.push ~t:producer ~item:payload () >>= fun r ->
          let position = get_ok r in
          Producer.advance ~t:producer ~position () >>= fun unit ->
          get_ok unit;
          push (m - 1) in
        push batch >>= fun () ->
        let rec pop = function
        | 0 -> return ()
        | m ->
          Consumer.pop ~t:consumer () >>= fun r ->
          let consumer_val, buffer = get_ok r in
          assert_equal ~printer:(fun x -> x) payload buffer;
	  Consumer.advance ~t:consumer ~position:consumer_val () >>= fun r ->
          get_ok r;
          pop (m - 1) in
        pop batch >>= fun () ->
        loop (n - 1) in
    (* push/pop 2 * the number of sectors to guarantee we experience some wraparound *)
    let open Lwt in
    loop Int64.(to_int (mul 2L (div size 512L))) >>= fun () ->
    finished := true;

    (* count how many items we can push in total *)
    let rec loop acc =
      let open Lwt in
      Producer.push ~t:producer ~item:payload () >>= function
      | Error `Retry -> return acc
      | Error _ -> failwith "counting the number of pushes"
      | Ok position ->
        Producer.advance ~t:producer ~position () >>= fun unit ->
        get_ok unit;
        loop (acc + 1) in
    loop 0 >>= fun n ->
    let expected = Int64.(to_int (div (sub size 1536L) (logand (lognot 3L) (of_int (String.length payload + 7))))) in
    assert_equal ~printer:string_of_int expected n;
    Producer.detach producer >>= fun () ->
    Consumer.detach consumer >>= fun () ->
    return () in
  Lwt_main.run t

let test_suspend () =
  let t =
    fresh_file size
    >>= fun name ->

    Block.connect name >>= fun disk ->
    let open Lwt_result in

    Producer.create ~disk () >>= fun () ->
    Producer.attach ~client:"test" ~queue:"test_suspend" ~disk () >>= fun producer ->
    Consumer.attach ~client:"test" ~queue:"test_suspend" ~disk () >>= fun consumer ->
    (* consumer thinks the queue is running *)
    let open Lwt in
    Consumer.state consumer >>= fun r ->
    assert_equal `Running (get_ok r);
    (* so does the producer *)
    Producer.state producer >>= fun r ->
    assert_equal `Running (get_ok r);
    (* consumer requests a suspend *)
    Consumer.suspend consumer >>= fun r ->
    get_ok r;
    (* it is not possible to request a resume before the ack *)
    Consumer.resume consumer >>= fun r ->
    assert_equal `Retry (get_error r);
    (* but the producer hasn't seen it so the queue is still running *)
    Consumer.state consumer >>= fun r ->
    assert_equal `Running (get_ok r);
    (* when the producer looks, it sees the suspend *)
    Producer.state producer >>= fun r ->
    assert_equal `Suspended (get_ok r);
    (* now the consumer sees the producer has suspended *)
    Consumer.state consumer >>= fun r ->
    assert_equal `Suspended (get_ok r);
    Consumer.resume consumer >>= fun r ->
    get_ok r;
    (* The queue is still suspended until the producer acknowledges *)
    Consumer.state consumer >>= fun r ->
    assert_equal `Suspended (get_ok r);
    (* The queue cannot be re-suspended until the producer has seen the resume *)
    Consumer.suspend consumer >>= fun r ->
    assert_equal `Retry (get_error r);
    (* The producer notices it immediately too *)
    Producer.state producer >>= fun r ->
    assert_equal `Running (get_ok r);
    
    return () in
  Lwt_main.run t

let test_suspend_advance_interleaved () =
  let t =
    fresh_file size
    >>= fun name ->


    Block.connect name >>= fun disk ->
    let open Lwt_result in
    Producer.create ~disk () >>= fun () ->
    Producer.attach ~client:"test" ~queue:"test_suspend" ~disk () >>= fun producer ->
    Consumer.attach ~client:"test" ~queue:"test_suspend" ~disk () >>= fun consumer ->

    (* put some data in the queue and get it ready to be advanced *)
    Producer.push ~t:producer ~item:"produce_consume_loop\n" () >>= fun position ->
    Producer.advance ~t:producer ~position () >>= fun () ->
    Consumer.pop ~t:consumer () >>= fun (position, _) ->

    let consumer_advanced_t, consumer_advanced_u = Lwt.wait () in

    let advance_consumer () =
      Consumer.advance ~t:consumer ~position () >>= fun () ->
      Lwt.wakeup_later consumer_advanced_u ();
      return ()
    in

    let open Lwt in

    (* advance the ring and wait for cache to be read *)
    Lwt.async advance_consumer;
    Lwt_unix.sleep 0.01 >>= fun () ->

    (* consumer requests a suspend *)
    Consumer.suspend consumer >>= fun r ->
    get_ok r;

    (* wait for advance to flush *)
    consumer_advanced_t >>= fun () ->

    Producer.state producer >>= fun r ->
    assert_bool "Suspend bit not seen by Producer" ((get_ok r) = `Suspended);

    return () in
  Lwt_main.run t

let test_journal () =
  let t =
    fresh_file size
    >>= fun name ->
    Block.connect name >>= fun device ->
    Mclock.connect () >>= fun _clock ->
    let module J = Shared_block.Journal.Make(Log)(Block)(Time)(Mclock)(Op) in
    let open Lwt_result in
    let perform xs =
      List.iter (fun x ->
        if x <> "hello"
        then failwith (Printf.sprintf "[%s]<>\"hello\"" (String.escaped x))
      ) xs;
      return (Ok ()) in
    J.start ~client:"test" ~name:"test_journal" device perform
    >>= fun j ->
    J.push j "hello"
    >>= fun w ->
    let open Lwt in
    w.J.sync ()
    >>= fun () ->
    J.push j (Bytes.create (Int64.to_int size) |> Bytes.to_string)
    >>= fun t ->
    ignore(get_error t);
    J.shutdown j
    >>= fun () ->
    J.push j ""
    >>= fun t ->
    ignore(get_error t);
    return () in
  Lwt_main.run t

let test_journal_replay () =
  let t =
    fresh_file size
    >>= fun name ->


    Block.connect name >>= fun device ->
    let open Lwt_result in
    let module J = Shared_block.Journal.Make(Log)(Block)(Time)(Mclock)(Op) in
    let attempts = Hashtbl.create 2 in
    let perform = function
      | [] -> return (Ok ())
      | x::_ ->
        let init = try Hashtbl.find attempts x with _ -> 0 in
        Hashtbl.replace attempts x (init+1);
        fail Not_found in
    J.start ~client:"test" ~name:"test_journal_replay" ~retry_interval:(Duration.of_f 0.1) device perform
    >>= fun j ->
    let open Lwt in
    J.push j "hello"
    >>= fun _ ->
    J.push j "hello2"
    >>= fun _ ->
    Lwt_unix.sleep 1.0 >>= fun () ->
    let hello_tries = try Hashtbl.find attempts "hello" with _ -> 0 in
    let hello2_tries = try Hashtbl.find attempts "hello2" with _ -> 0 in

    let hello_tries_ok = hello_tries > 1 in
    let hello2_tries_ok = hello2_tries = 0 in
    Printf.fprintf stderr "hello_tries = %d hello2_tries = %d\n%!" hello_tries hello2_tries;
    assert_equal ~printer:string_of_bool hello_tries_ok true;
    assert_equal ~printer:string_of_bool hello2_tries_ok true;
    let open Lwt in
    J.shutdown j
    >>= fun () ->
    let open Lwt_result in
    (* Now pretend that we've just crashed and restarted *)
    let ok = ref false in
    let perform _ = ok := true; return (Ok ()) in
    J.start ~client:"test" ~name:"test_journal_replay" device perform
    >>= fun j ->
    (* The operation should have been performed *)
    assert_equal ~printer:string_of_bool true !ok;
    J.shutdown j in
  Lwt_main.run t

(* Check that items are processed from the journal in the order they
   went in. *)
let test_journal_order () =
  let t =
    (* Make it only big enough for a few items *)
    fresh_file (Int64.of_int (512 + 512 + 512 + 512 + 8))
    >>= fun name ->

    Block.connect name >>= fun device ->

    let open Lwt_result in

    let module J = Shared_block.Journal.Make(Log)(Block)(Time)(Mclock)(Int) in

    let counter = ref 0l in
    let interval = Duration.of_sec @@ 5 in
    let perform xs =
      let rec loop = function
      | [] -> return (Ok ())
      | x :: xs ->
        assert_equal ~printer:Int32.to_string !counter x;
        counter := Int32.succ !counter;
        loop xs in
      loop xs
      >>= fun () ->
      return (Ok ()) in
    J.start ~client:"test" ~name:"test_journal_order" ~flush_interval:interval device perform
    >>= fun j ->
    let rec loop = function
      | 1000l ->
        J.push j 1000l
      | n ->
        J.push j n
        >>= fun _ ->
        loop Int32.(succ n) in
    loop 0l
    >>= fun w ->
    let open Lwt in
    w.J.sync ()
    >>= fun () ->
    J.shutdown j in
  Lwt_main.run t

let rec allpairs xs ys = match xs with
  | [] -> []
  | x :: xs -> List.map (fun y -> x, y) ys @ (allpairs xs ys)

let _ =
  let test_push_pops = List.map (fun (length, batch) ->
    Printf.sprintf "push pop %d bytes in batches of %d" length batch >:: (test_push_pop length batch)
  ) (allpairs interesting_lengths interesting_batch_sizes) in

  let suite = "shared-block-ring" >::: [
    "test suspend" >:: test_suspend;
    "test suspend and advance interleaved" >:: test_suspend_advance_interleaved;
    "test journal" >:: test_journal;
    "test journal replay" >:: test_journal_replay;
    "test journal order" >:: test_journal_order;
  ] @ test_push_pops in
  run_test_tt_main suite
