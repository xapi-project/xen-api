open Bechamel
open Xapi_stdext_encodings.Encodings

let test name f =
  Test.make_indexed_with_resource ~name ~args:[10; 1000; 10000]
    Test.multiple (* TODO: Test.uniq segfaults here, bechamel bug *)
    ~allocate:(fun i -> String.make i 'x')
    ~free:ignore
    (fun (_ : int) -> Staged.stage f)

let benchmarks =
  Test.make_grouped ~name:"Encodings.validate"
    [test "UTF8_XML" UTF8_XML.validate]

let () = Bechamel_simple_cli.cli benchmarks
