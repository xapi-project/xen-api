#use "xcp.ml";;
#use "storage.ml";;
module S = (SR_test(Lwt): SR with type 'a t = 'a Lwt.t);;
module S_d = SR_server_dispatcher(S);;
module C = SR_client(S_d);;

lwt x = C.ls (Types.SR.Ls.In.( { dbg = "dbg" }));;

