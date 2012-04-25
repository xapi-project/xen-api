

let test host port =
	try
        let test_str1 = String.make 512 'a' in
        let test_str2 = String.make 512 'b' in
        let test_str3 = String.make 512 'c' in
        let test_str4 = String.make 512 'd' in
        let test_str5 = String.make 512 'e' in
        let test_str6 = String.make 512 'f' in
        let test_str7 = String.make 512 'g' in
        let test_str8 = String.make 512 'h' in
		Printf.printf "Connecting...\n";
		let (sock,sz,flags) = Nbd.connect host port in
        Printf.printf "Connected: size=%Ld\n" sz;
        let _ = Nbd.write sock test_str1 0L in
        let _ = Nbd.write sock test_str2 512L in
        let _ = Nbd.write sock test_str3 1024L in
        let _ = Nbd.write sock test_str4 1536L in
        let _ = Nbd.write sock test_str5 2048L in
        let _ = Nbd.write sock test_str6 2560L in
        let _ = Nbd.write sock test_str7 3072L in
        let _ = Nbd.write sock test_str8 3584L in

        Printf.printf "Written\n";
        let Some str2 = Nbd.read sock 0L 4096l in
        Printf.printf "%s\n" str2;
		()
    with e -> 
		Printf.printf "Caught exception: %s" (Printexc.to_string e);
		()

let _ =
	test Sys.argv.(1) (int_of_string Sys.argv.(2))

