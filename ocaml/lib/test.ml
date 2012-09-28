#use "xcp.ml";;
#use "storage.ml";;

module S = (SR_test(Lwt): SR with type 'a t = 'a Lwt.t);;
module S_d = SR_server_dispatcher(S);;
module SR = SR_client(S_d);;


module V = (VDI_test(Lwt): VDI with type 'a t = 'a Lwt.t);;
module V_d = VDI_server_dispatcher(V);;
module VDI = VDI_client(V_d);;

