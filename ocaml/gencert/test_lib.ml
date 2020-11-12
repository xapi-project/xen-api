let () =
  Alcotest.run "Gencert Library"
    [("Validation tests", Test_validation.all); ("Pem tests", Test_pem.all)]
