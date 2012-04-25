

let test host port =
	try_lwt
        let test_str1 = String.make 512 'a' in
        let test_str2 = String.make 512 'b' in
        let test_str3 = String.make 512 'c' in
        let test_str4 = String.make 512 'd' in
        let test_str5 = String.make 512 'e' in
        let test_str6 = String.make 512 'f' in
        let test_str7 = String.make 512 'g' in
        let test_str8 = String.make 512 'h' in
		Printf.printf "Connecting...\n";
		lwt (sock,sz,flags) = Nbd_lwt.connect host port in
        Printf.printf "Connected: size=%Ld\n" sz;
        let t1 = Nbd_lwt.write sock test_str1 0L in
        let t2 = Nbd_lwt.write sock test_str2 512L in
        let t3 = Nbd_lwt.write sock test_str3 1024L in
        let t4 = Nbd_lwt.write sock test_str4 1536L in
        let t5 = Nbd_lwt.write sock test_str5 2048L in
        let t6 = Nbd_lwt.write sock test_str6 2560L in
        let t7 = Nbd_lwt.write sock test_str7 3072L in
        let t8 = Nbd_lwt.write sock test_str8 3584L in

		lwt () = Lwt.join [t1; t2; t3; t4; t5; t6; t7; t8] in
        Printf.printf "Written\n";
        lwt str2 = Nbd_lwt.read sock 0L 4096l in
        Printf.printf "%s\n" str2;
        Lwt.return ()
    with e -> 
		Printf.printf "Caught exception: %s" (Printexc.to_string e);
		Lwt.return ()

let _ =
	Lwt_main.run (test Sys.argv.(1) (int_of_string Sys.argv.(2)))

