open OUnit2
module J = Ezjsonm

let (^/) = Filename.concat

let specs_directory =
  Filename.parent_dir_name ^/ "specs"

type test = {
  from_file: string;
  name: string;
  data: J.t;
  partials: (Mustache.name * Mustache.t) list;
  template: string;
  expected: string;
  desc: string;
}

let apply_mustache test =
  let tmpl =
    try Mustache.of_string test.template
    with e ->
      Printf.eprintf "Parsing of test %s from %s failed.\n"
        test.name test.from_file;
      raise e
  in
  let partials name =
    try Some (List.assoc name test.partials)
    with Not_found -> None
  in
  let rendered =
    try Mustache.render ~strict:false ~partials tmpl test.data
    with e ->
      Printf.eprintf "Rendering of test %s from %s failed.\n"
        test.name test.from_file;
      raise e
  in
  rendered

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let j_of_data : J.value -> J.t = function
  | `O l -> `O l
  | _ -> failwith "Incorrect json data"

let load_test_file f =
  let test_j =
    load_file (specs_directory ^/ f)
    |> J.from_string
    |> J.value
  in

  let test_of_json j =
    let test_name = J.find j ["name"] |> J.get_string in
    let parse_partials j =
      if not (J.mem j ["partials"]) then
        []
      else
        J.find j ["partials"]
        |> J.get_dict
        |> List.map (fun (name, tmpl) ->
          try (name, Mustache.of_string (J.get_string tmpl))
          with e ->
            Printf.eprintf "Parsing of partial %s of test %s failed\n"
              name test_name;
            raise e
        )
    in
    {
      from_file = f;
      name = test_name;
      data = J.find j ["data"] |> j_of_data;
      partials = parse_partials j;
      template = J.find j ["template"] |> J.get_string;
      expected = J.find j ["expected"] |> J.get_string;
      desc = J.find j ["desc"] |> J.get_string;
    }
  in
  J.find test_j ["tests"] |> J.get_list test_of_json

let assert_equal x y =
  fun _ -> assert_equal ~printer:(fun s -> s) x y

let mktest test =
  test.name >::
  assert_equal test.expected (apply_mustache test)

let specs = [
  "comments.json";
  "interpolation.json";
  "inverted.json";
  "partials.json";
  "sections.json";
]

let tests =
  "Mustache specs test suite" >:::
  List.map (fun specfile ->
    ("Test suite from " ^ specfile) >:::
    (load_test_file specfile |> List.map mktest)
  ) specs

let () =
  run_test_tt_main tests
