open Threadext
open Pervasiveext

let user_agent = "test_client"

(* To do:
   1. test with and without SSL
   2. test with n parallel threads
   3. make sure xapi still works
   4. make xapi able to read stats

*)

let with_connection ip port f =
    let inet_addr = Unix.inet_addr_of_string ip in
	let addr = Unix.ADDR_INET(inet_addr, port) in
	let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect s addr;
	Unixext.set_tcp_nodelay s true;
	finally
		(fun () -> f s)
		(fun () -> Unix.close s)

let with_stunnel ip port =
	let done_init = ref false in
	fun f ->
		if not !done_init then Stunnel.init_stunnel_path ();
		let s = Stunnel.connect ~use_fork_exec_helper:false ~extended_diagnosis:false ip port in
		let fd = s.Stunnel.fd in
		finally
			(fun () -> f fd)
			(fun () -> Stunnel.disconnect s)

let one keep_alive s =
	Http_client.rpc s (Http.Request.make ~version:"1.1" ~keep_alive
		~user_agent ~body:"hello" Http.Post "/echo")
		(fun response s ->
			match response.Http.Response.content_length with
				| Some l ->
					let (_: string) = Unixext.really_read_string s (Int64.to_int l) in
(*
					Printf.printf "Read [%s]\n" x;
					flush stdout
*)
					()
				| None -> failwith "Need a content length"
		)

module Normal_population = struct
  (** Stats on a normally-distributed population *)
  type t = { sigma_x: float;
	     sigma_xx: float;
	     n: int }

  let empty = { sigma_x = 0.; sigma_xx = 0.; n = 0 }

  let sample (p: t) (x: float) : t =
    { sigma_x = p.sigma_x +. x;
      sigma_xx = p.sigma_xx +. x *. x;
      n = p.n + 1 }

  exception Unknown

  let mean (p: t) : float = p.sigma_x /. (float_of_int p.n)
  let sd (p: t) : float =
    if p.n = 0
    then raise Unknown
    else
      let n = float_of_int p.n in
      sqrt (n *. p.sigma_xx -. p.sigma_x *. p.sigma_x) /. n

  let to_string (p: t) = Printf.sprintf "%.1f +/- %.1f" (mean p) (sd p)
end

let per_nsec n f =
	let start = Unix.gettimeofday () in
	let t = ref 0 in
	while Unix.gettimeofday () -. start < n do
		f ();
		incr t
	done;
	int_of_float (float_of_int !t /. n)

let threads n f =
	let results = Array.make n 0 in
	let body i () = results.(i) <- f () in
	let threads = Array.mapi (fun i _ -> Thread.create (body i) ()) results in
	Array.iter Thread.join threads;
	Array.fold_left (+) 0 results

let sample n f =
	let p = ref Normal_population.empty in
	for i = 1 to n do
		let v = f () in
		p := Normal_population.sample !p (float_of_int v);
	done;
	!p

let _ =
	let ip = ref "127.0.0.1" in
	let port = ref 8080 in
	let use_ssl = ref false in
	Arg.parse [
		"-ip", Arg.Set_string ip, "IP to connect to";
		"-p", Arg.Set_int port, "port to connect";
		"--ssl", Arg.Set use_ssl, "use SSL rather than plaintext";
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s\n" x)
		"A simple test HTTP client";

	let transport = if !use_ssl then with_stunnel else with_connection in
(*
	Printf.printf "Overhead of timing:                ";
	let overhead = sample 10 (fun () -> per_nsec 1. (fun () -> ())) in
	Printf.printf "%s ops/sec\n" (Normal_population.to_string overhead);
*)
	Printf.printf "Persistent connection:             ";
	let persistent = sample 10
		(fun () -> transport !ip !port
			(fun s -> per_nsec 1. (fun () -> one true s))) in
	Printf.printf "%s RPCs/sec\n" (Normal_population.to_string persistent);
	Printf.printf "Non-persistent connections:        ";
	let nonpersistent = sample 10
		(fun () -> per_nsec 1.
			(fun () -> transport !ip !port (one false))) in
	Printf.printf "%s RPCs/sec\n" (Normal_population.to_string nonpersistent);
	let thread_persistent =
		sample 5
			(fun () ->
				threads 10
					(fun () ->
						transport !ip !port
							(fun s ->
								per_nsec 5.
									(fun () -> one true s)
							)
					)
			) in
	Printf.printf "10 threads persistent connections: ";
	Printf.printf "%s RPCs/sec\n" (Normal_population.to_string thread_persistent);

