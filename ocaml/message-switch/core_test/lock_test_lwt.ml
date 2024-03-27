open Message_switch_lwt

let test_lwt_lock = Protocol_lwt.Mtest.mutex_provides_mutal_exclusion ()

let () = Lwt_main.run test_lwt_lock
