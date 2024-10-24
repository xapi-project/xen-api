open Bechamel

let () = Uuidx.make_default := Uuidx.make_uuid_fast

let benchmarks =
  Test.make_grouped ~name:"uuidx creation"
    [
      Test.make ~name:"Uuidx.make_uuid_urnd" (Staged.stage Uuidx.make_uuid_urnd)
    ; Test.make ~name:"Uuidx.make" (Staged.stage Uuidx.make)
    ]

let () = Bechamel_simple_cli.cli benchmarks
