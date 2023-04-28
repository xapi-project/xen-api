module Make (S : STM.Spec) (L : Lin.Spec) = struct
  module S_domain = STM_thread.Make (S)
  module L_domain = Lin_thread.Make (L)

  let tests ~count ~name =
    [
      S_domain.agree_test_conc ~count ~name:(name ^ "(STM)")
    ; L_domain.lin_test ~count ~name:(name ^ " (Lin)")
    ]
end
