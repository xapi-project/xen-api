let product_version () = Inventory.lookup ~default:"" "PRODUCT_VERSION"

let product_version_text () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT"

let product_version_text_short () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT_SHORT"

let platform_name () = Inventory.lookup ~default:"" "PLATFORM_NAME"

let platform_version () = Inventory.lookup ~default:"0.0.0" "PLATFORM_VERSION"

let product_brand () = Inventory.lookup ~default:"" "PRODUCT_BRAND"

let build_number () = Inventory.lookup ~default:"" "BUILD_NUMBER"

let hostname = "localhost"

let date = Xapi_build_info.date

let parse_xapi_version version =
  try
    Some (Scanf.sscanf version "%d.%d.%s" (fun maj min rest -> (maj, min, rest)))
  with _ -> None

let version, xapi_version_major, xapi_version_minor, git_id =
  match Build_info.V1.version () with
  | None ->
      ("0.0.dev", 0, 0, "dev")
  | Some v -> (
      let str = Build_info.V1.Version.to_string v in
      let version =
        if String.starts_with ~prefix:"v" str then
          String.sub str 1 (String.length str - 1)
        else
          str
      in
      match parse_xapi_version version with
      | Some (maj, min, git_id) ->
          (version, maj, min, git_id)
      | None ->
          Printf.eprintf
            "Unusable xapi version '%s': either no version was configured, or \
             the configured one is invalid.\n\
             To configure one, use a command like:\n\
            \  ./configure --xapi_version=<major>.<minor>.<patch>\n"
            version ;
          exit 2
    )

let compare_to_local v =
  let maj, min, _ =
    match parse_xapi_version v with
    | Some parsed ->
        parsed
    | None ->
        failwith
          (Printf.sprintf "Couldn't determine xapi version from string: '%s'" v)
  in
  let ( <?> ) a b =
    if a = 0 then
      b
    else
      a
  in
  Int.compare xapi_version_major maj
  <?> Int.compare xapi_version_minor min
  <?> 0

let xapi_user_agent =
  "xapi/"
  ^ string_of_int xapi_version_major
  ^ "."
  ^ string_of_int xapi_version_minor

let arg_spec =
  ( "--version"
  , Arg.Unit (fun () -> print_endline version ; exit 0)
  , " Print the version and exit"
  )
