module Make (S : STM.Spec) (L : Lin.Spec) = struct
  module S_domain = STM_domain.Make (S)
  module L_domain = Lin_domain.Make (L)

  let tests ~count ~name =
    [
      S_domain.agree_test_par ~count ~name:(name ^ " STM domain")
    ; L_domain.lin_test ~count ~name:(name ^ " Lin domain")
    ]
end
