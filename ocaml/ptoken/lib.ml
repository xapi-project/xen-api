let gen_token () =
  let uuid () = Uuid.to_string (Uuid.make_uuid ()) in
  let uuids = String.concat "/" [uuid (); uuid (); uuid ()] in
  SecretString.of_string uuids
