

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
        let _ = Nbd.write sock 0L test_str1 0 (String.length test_str1) in
        let _ = Nbd.write sock 512L test_str2 0 (String.length test_str2)  in
        let _ = Nbd.write sock 1024L test_str3 0 (String.length test_str3)  in
        let _ = Nbd.write sock 1536L test_str4 0 (String.length test_str4)  in
        let _ = Nbd.write sock 2048L test_str5 0 (String.length test_str5)  in
        let _ = Nbd.write sock 2560L test_str6 0 (String.length test_str6)  in
        let _ = Nbd.write sock 3072L test_str7 0 (String.length test_str7)  in
        let _ = Nbd.write sock 3584L test_str8 0 (String.length test_str8)  in

        Printf.printf "Written\n";
        let Some str2 = Nbd.read sock 0L 4096l in
        Printf.printf "%s\n" str2;
		()
    with e -> 
		Printf.printf "Caught exception: %s" (Printexc.to_string e);
		()

let _ =
	test Sys.argv.(1) (int_of_string Sys.argv.(2))

