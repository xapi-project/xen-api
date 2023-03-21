open Xapi_metastore

let dir = "test_metastore_random_configs"

let unit_error = Alcotest.(result unit @@ of_pp Rresult.R.pp_msg)

let test_config path () =
  let actual =
    path
    |> Serialization.string_of_file_exn
    |> Config.deserialize
    |> Result.map ignore
  and expected = Ok () in
  Alcotest.check' unit_error ~msg:"check that we can deserialize an old value"
    ~actual ~expected

let tests =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter_map (fun name ->
         if name <> "." && name <> ".." then
           Some Fpath.(v dir / name)
         else
           None
     )
  |> List.map @@ fun path -> (Fpath.to_string path, `Quick, test_config path)

let () = Alcotest.run __MODULE__ [("Xapi_metastore.config", tests)]
