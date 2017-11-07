(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)
Ocamlbuild_plugin.dispatch
  (fun hook ->
     dispatch_default hook ;
     Ocamlbuild_cppo.dispatcher hook ;
     Bisect_ppx_plugin.dispatch hook ;
  );;
