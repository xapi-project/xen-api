(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module Operations = struct
  let count = Atomic.make 0
  type witness = unit
  let label () = "operations"
  let unit () = "op"
  let make () = ()
  let load () = ()
  let unload () = ()
  let get () = Atomic.get count |> float_of_int
end

module Extension = struct
  let operations = Bechamel.Measure.register (module Operations)
end

let operations = Bechamel.Measure.instance (module Operations) Extension.operations 

let semaphore () = Semaphore.Binary.make false

let wait = Semaphore.Binary.acquire

let signal = Semaphore.Binary.release

(* this uses the 'barrier binary array' implementation technique, see benchmarks in {!mod:Ezbechamel_basics}. *)
module Worker = struct
  type 'a t = {
      initialized: Semaphore.Binary.t
    ; start: Semaphore.Binary.t
    ; stopped: Semaphore.Binary.t
    ; quit: bool Atomic.t
    ; result: (unit, Rresult.R.exn_trap) result option Atomic.t
  }

  let ok = Some (Ok ())

  let worker ~allocate ~free ~run t =
    let resource = allocate () in
    let finally () = free resource in
    let rec worker_loop () =
      wait t.start ;
      if not (Atomic.get t.quit) then (
        let () =
          try
            let () = Sys.opaque_identity (Bechamel.Staged.unstage run resource) in
            Atomic.set t.result ok;
            Atomic.incr Operations.count;
          with e ->
            let bt = Printexc.get_raw_backtrace () in
            Atomic.set t.result (Some (Error (`Exn_trap (e, bt))))
        in
        signal t.stopped ; worker_loop ()
      )
    in
    signal t.initialized;
    Fun.protect ~finally worker_loop

  let make ~allocate ~free ~run _ =
    let start = semaphore () and stopped = semaphore () and initialized = semaphore () in
    let t = {start; stopped;initialized; quit= Atomic.make false; result = Atomic.make None} in
    (t, Thread.create (worker ~allocate ~free ~run) t)

  let signal_start (t, _) = signal t.start

  let wait_stop (t, _) = wait t.stopped

  let set_quit (t, _) =
    let ok = Atomic.compare_and_set t.quit false true in
    assert ok (* detect double free *)

  let join_thread (_, thread) = Thread.join thread

  let check_exception (t, _) =
    match Atomic.get t.result with
    | None -> assert false (* it has signaled the barrier, it must've finished *)
    | Some (Ok ()) -> ()
    | Some (Error (`Exn_trap (e, bt))) -> Printexc.raise_with_backtrace e bt

  let barrier_wait all =
    (* must first start all, do not combine this with waiting *)
    Array.iter signal_start all ;

    (* wait until all are finished, benchmark code is running in the thread now *)
    Array.iter wait_stop all;

    Array.iter check_exception all

  let wait_initialized (t, _) =
    wait t.initialized

  let wait_init all =
    Array.iter wait_initialized all

  let shutdown all =
    Array.iter set_quit all ;
    Array.iter signal_start all ;
    Array.iter join_thread all
end

let test_concurrently ?(threads = [1; 2; 4; 8; 16]) ~allocate ~free ~name run =
  let open Bechamel in
  let allocate n =
    let t = Array.init n @@ Worker.make ~allocate ~free ~run in
    (* wait until all threads have initialized,
       Note: do not run a full cycle here on the regular barrier, because that will hide problems like the sleep call in crypt_r which only happens on first use.
     *)
    Worker.wait_init t; t
  in
  let free all = Worker.shutdown all in
  Test.make_indexed_with_resource ~name ~args:threads Test.multiple ~allocate
    ~free (fun _ -> Staged.stage Worker.barrier_wait
  )
