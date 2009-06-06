let stdout_m = Mutex.create () 

let debug ?(out=stdout) (fmt: ('a , unit, string, unit) format4) =
  Threadext.Mutex.execute stdout_m
    (fun () ->
       Printf.kprintf (fun s -> Printf.fprintf out "%s\n" s; flush stdout) fmt 
    )
