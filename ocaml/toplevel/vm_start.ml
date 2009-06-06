open Toplevelhelper

let _ =
  host := "mindanao";
  port := 8086;
  let s = init_session "root" "xenroot" in
  let vm = List.nth (Remote.VM.get_by_name_label s Sys.argv.(1)) 0 in
  Remote.VM.start s vm false
