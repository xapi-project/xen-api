(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Tracing
open Tracing_export

module D = Debug.Make (struct let name = "test_observer" end)

open D
module ComponentSet = Set.Make (Xapi_observer_components)

let component_set_to_string cs =
  cs
  |> ComponentSet.to_seq
  |> Seq.map Xapi_observer_components.to_string
  |> List.of_seq
  |> String.concat ","

let component_set_testable =
  let pp = Fmt.of_to_string component_set_to_string in

  Alcotest.testable pp ComponentSet.equal

let () = Printexc.record_backtrace true

let trace_log_dir ?(test_name = "") () =
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "%s/var/log/dt/zipkinv2/json/" test_name)

let () =
  Destination.File.set_trace_log_dir (trace_log_dir ()) ;
  set_service_name "unit_tests"

module Xapi_DB = struct
  let assert_num_observers ~__context x =
    let observers = Db.Observer.get_all ~__context in
    Alcotest.(check int)
      (Printf.sprintf "%d observer(s) exists in DB" x)
      x (List.length observers)

  let assert_observer_disabled ~__context ~self =
    let enabled = Db.Observer.get_enabled ~__context ~self in
    Alcotest.(check bool) "Observer disabled" false enabled

  let check_endpoints ~__context ~self ~endpoints =
    let db_endpoints = Db.Observer.get_endpoints ~__context ~self in
    List.iter2
      (fun x y -> Alcotest.(check string) "Observer contains endpoint" x y)
      endpoints db_endpoints
end

module TracerProvider = struct
  let assert_num_observers ~__context x =
    let providers = TracerProvider.get_tracer_providers () in
    Alcotest.(check int)
      (Printf.sprintf "%d provider(s) exists in lib " x)
      x (List.length providers)

  let find_provider_exn ~name =
    let providers = TracerProvider.get_tracer_providers () in
    match
      List.find_opt (fun x -> TracerProvider.get_name_label x = name) providers
    with
    | Some provider ->
        provider
    | None ->
        Alcotest.failf "No provider with the name %s" name

  let assert_observer_disabled ~name =
    let provider = find_provider_exn ~name in
    Alcotest.(check bool)
      "Provider disabled" false
      (TracerProvider.get_enabled provider)

  let assert_mandatory_attributes ~name =
    let provider = find_provider_exn ~name in
    let tags = TracerProvider.get_attributes provider in
    List.iter
      (fun x ->
        try
          let _ = List.assoc x tags in
          ()
        with _ -> Alcotest.failf "Missing mandatory attribute: %s" x
      )
      [
        "xs.pool.uuid"
      ; "xs.host.name"
      ; "xs.host.uuid"
      ; "xs.observer.name"
      ; "service.name"
      ]

  let check_endpoints ~name ~endpoints =
    let provider = find_provider_exn ~name in
    let provider_endpoints =
      TracerProvider.get_endpoints provider
      |> List.map (fun endpoint ->
             match endpoint with
             | Bugtool ->
                 "bugtool"
             | Url x ->
                 Uri.to_string x
         )
    in
    List.iter2
      (fun x y -> Alcotest.(check string) "Observer contains endpoint" x y)
      endpoints provider_endpoints
end

let test_destroy ~__context ~self () =
  try
    Xapi_observer.unregister ~__context ~self ~host:!Xapi_globs.localhost_ref ;
    Xapi_observer.destroy ~__context ~self
  with _ -> debug "Attempted to destroy an observer that does not exist"

let test_create ~__context ?(name_label = "test-observer") ?(enabled = false) ()
    =
  let self =
    Xapi_observer.create ~__context ~name_label ~name_description:"" ~hosts:[]
      ~attributes:[] ~endpoints:["bugtool"] ~components:["xapi"] ~enabled
  in
  let host = !Xapi_globs.localhost_ref in
  Xapi_observer.register ~__context ~self ~host ;
  self

let start_test_span () =
  let tracer = Tracer.get_tracer ~name:"test-observer" in
  let span = Tracer.start ~tracer ~name:"test_task" ~parent:None () in
  span

let start_test_trace () =
  let tracer = Tracer.get_tracer ~name:"test-observer" in
  let root =
    Tracer.start ~tracer ~name:"test_task" ~parent:None ()
    |> Result.value ~default:None
  in
  let span1 =
    Tracer.start ~tracer ~name:"test_task" ~parent:root ()
    |> Result.value ~default:None
  in
  let span2 =
    Tracer.start ~tracer ~name:"test_task" ~parent:span1 ()
    |> Result.value ~default:None
  in
  let span3 =
    Tracer.start ~tracer ~name:"test_task" ~parent:root ()
    |> Result.value ~default:None
  in
  let span4 =
    Tracer.start ~tracer ~name:"test_task" ~parent:span3 ()
    |> Result.value ~default:None
  in
  let spans = [span2; span1; span3; span4; root] in
  spans

let test_observer_create_and_destroy () =
  let __context = Test_common.make_test_database () in
  Xapi_DB.assert_num_observers ~__context 0 ;
  TracerProvider.assert_num_observers ~__context 0 ;

  let name = "test-observer" in
  let self = test_create ~__context () ~name_label:name in
  Xapi_DB.assert_num_observers ~__context 1 ;
  TracerProvider.assert_num_observers ~__context 1 ;

  Xapi_DB.assert_observer_disabled ~__context ~self ;
  TracerProvider.assert_observer_disabled ~name ;

  (* DB has no mandatory attributes *)
  TracerProvider.assert_mandatory_attributes ~name ;

  let name2 = "test-observer-2" in
  let self2 = test_create ~__context ~name_label:name2 () in
  Xapi_DB.assert_num_observers ~__context 2 ;
  TracerProvider.assert_num_observers ~__context 2 ;

  test_destroy ~__context ~self:self2 () ;
  Xapi_DB.assert_num_observers ~__context 1 ;
  TracerProvider.assert_num_observers ~__context 1 ;

  test_destroy ~__context ~self () ;
  Xapi_DB.assert_num_observers ~__context 0 ;
  TracerProvider.assert_num_observers ~__context 0

let test_hosts ~__context ~self =
  let valid_host = !Xapi_globs.localhost_ref in
  ( try Xapi_observer.set_hosts ~__context ~self ~value:[valid_host]
    with _ -> Alcotest.failf "Failed to set valid host"
  ) ;

  let invalid_host = Ref.null in
  Alcotest.check_raises "Xapi_observer.set_hosts should fail on invalid host"
    Api_errors.(
      Server_error (invalid_value, ["host"; Ref.string_of invalid_host])
    )
    (fun () ->
      Xapi_observer.set_hosts ~__context ~self ~value:[invalid_host] |> ignore
    )

let test_components ~__context ~self =
  let valid_components = ["xapi"] in
  List.iter
    (fun x ->
      try Xapi_observer.set_components ~__context ~self ~value:[x]
      with _ -> Alcotest.failf "Failed to set valid component"
    )
    valid_components ;

  let invalid_component = "invalid-component" in
  Alcotest.check_raises
    "Xapi_observer.set_components should fail on invalid component"
    Api_errors.(Server_error (invalid_value, ["component"; invalid_component]))
    (fun () ->
      Xapi_observer.set_components ~__context ~self ~value:[invalid_component]
      |> ignore
    )

let test_endpoints ~__context ~self =
  Xapi_globs.observer_endpoint_http_enabled := true ;
  let valid_endpoints = ["bugtool"; "http://example.com:9411/api/v2/spans"] in
  List.iter
    (fun valid_endpoint ->
      try Xapi_observer.set_endpoints ~__context ~self ~value:[valid_endpoint]
      with _ -> Alcotest.failf "Failed to set valid endpoint"
    )
    valid_endpoints ;

  let invalid_endpoints = [""; "httb://badexample.com:9411/api/v2/spans"] in
  List.iter
    (fun invalid_endpoint ->
      Alcotest.check_raises
        "Xapi_observer.set_components should fail on invalid component"
        Api_errors.(Server_error (invalid_value, ["endpoint"; invalid_endpoint]))
        (fun () ->
          Xapi_observer.set_endpoints ~__context ~self ~value:[invalid_endpoint]
          |> ignore
        )
    )
    invalid_endpoints

let test_observer_valid_params () =
  let __context = Test_common.make_test_database () in
  let self = test_create ~__context ~name_label:"test-observer" () in

  test_hosts ~__context ~self ;
  test_components ~__context ~self ;
  test_endpoints ~__context ~self ;

  test_destroy ~__context ~self ()

let test_observer_endpoint () =
  let __context = Test_common.make_test_database () in
  let self = test_create ~__context () in
  let url = "http://example.com:9411/api/v2/spans" in
  let endpoints = [url] in
  Xapi_observer.set_endpoints ~__context ~self ~value:endpoints ;
  Xapi_DB.check_endpoints ~__context ~self ~endpoints ;
  TracerProvider.check_endpoints ~name:"test-observer" ~endpoints ;

  let endpoints = ["bugtool"; url] in
  Xapi_observer.set_endpoints ~__context ~self ~value:endpoints ;
  Xapi_DB.check_endpoints ~__context ~self ~endpoints ;
  TracerProvider.check_endpoints ~name:"test-observer" ~endpoints ;
  test_destroy ~__context ~self ()

let is_dir_empty ~test_trace_log_dir =
  try
    let log_dir =
      Sys.readdir test_trace_log_dir
      |> Array.map (Filename.concat test_trace_log_dir)
    in
    Array.length log_dir = 0
  with e ->
    debug "%s" (Printexc.to_string e) ;
    true

let clear_dir ~test_trace_log_dir () =
  try
    Xapi_stdext_unix.Unixext.rm_rec test_trace_log_dir ;
    Alcotest.(check bool)
      "log dir successfully cleared" true
      (is_dir_empty ~test_trace_log_dir)
  with _ -> ()

let verify_json_fields_and_values ~json =
  match json with
  | `Assoc
      [
        ( "tags"
        , `Assoc
            [
              ("xs.pool.uuid", `String _)
            ; ("xs.observer.name", `String "test-observer")
            ; ("xs.host.uuid", `String _)
            ; ("xs.host.name", `String _)
            ; ("service.name", `String _)
            ]
        )
      ; ("annotations", `List _)
      ; ("localEndpoint", `Assoc [("serviceName", `String "unit_tests")])
      ; ("duration", `Int _)
      ; ("timestamp", `Int _)
      ; ("name", `String "test_task")
      ; ("traceId", `String _)
      ; ("id", `String _)
      ] ->
      ()
  | _ ->
      Alcotest.failf "Trace log json format incorrect: %s"
        (Yojson.Basic.to_string json)

let assert_exported_files_contains_expected_json_fields_and_values
    ~test_trace_log_dir () =
  let log_dir =
    Sys.readdir test_trace_log_dir |> Array.map (fun x -> test_trace_log_dir ^ x)
  in
  Array.iter
    (fun x ->
      let json_file = Yojson.Basic.from_file x |> Yojson.Basic.Util.to_list in
      List.iter (fun json -> verify_json_fields_and_values ~json) json_file
    )
    log_dir

let test_file_export_writes () =
  let test_trace_log_dir =
    trace_log_dir ~test_name:"test_file_export_writes" ()
  in
  Destination.File.set_trace_log_dir test_trace_log_dir ;
  let __context = Test_common.make_test_database () in
  let self = test_create ~__context ~enabled:true () in
  clear_dir ~test_trace_log_dir () ;
  ( try
      let span = start_test_span () in
      match span with
      | Ok x -> (
          let _ = Tracer.finish x in
          Destination.flush_spans () ;
          Alcotest.(check bool)
            "tracing files written to disk when tracing enabled by default"
            false
            (is_dir_empty ~test_trace_log_dir) ;

          assert_exported_files_contains_expected_json_fields_and_values
            ~test_trace_log_dir () ;

          clear_dir ~test_trace_log_dir () ;

          let span = start_test_span () in
          Xapi_observer.set_enabled ~__context ~self ~value:false ;
          Xapi_DB.assert_observer_disabled ~__context ~self ;
          TracerProvider.assert_observer_disabled ~name:"test-observer" ;
          match span with
          | Ok x ->
              let _ = Tracer.finish x in
              Destination.flush_spans () ;
              Alcotest.(check bool)
                "tracing files not written when tracing disabled" true
                (is_dir_empty ~test_trace_log_dir)
          | Error e ->
              raise e
        )
      | Error e ->
          Alcotest.failf "Span start failed with %s" (Printexc.to_string e)
    with e -> Alcotest.failf "Error: %s" (Printexc.to_string e)
  ) ;
  clear_dir ~test_trace_log_dir () ;
  test_destroy ~__context ~self ()

let test_all_spans_finish () =
  let __context = Test_common.make_test_database () in
  let self = test_create ~__context () in
  let trace_spans = start_test_trace () in
  let active_spans, _ = Spans.dump () in
  let _ = List.map (fun span -> Tracer.finish span) trace_spans in
  let remaining_spans, finished_spans = Spans.dump () in
  let result =
    Hashtbl.fold
      (fun _k v acc -> snd finished_spans = List.length v && acc)
      active_spans true
  in
  Alcotest.(check bool)
    "All spans that are finished are moved to finished_spans" true result ;
  Alcotest.(check int)
    "traces with no spans are removed from the hashtable" 0
    (Hashtbl.length remaining_spans) ;
  test_destroy ~__context ~self ()

let test_hashtbl_leaks () =
  let test_trace_log_dir = trace_log_dir ~test_name:"test_hashtbl_leaks" () in
  let __context = Test_common.make_test_database () in
  let self = test_create ~__context ~enabled:true () in
  let filter_export_spans span =
    match String.lowercase_ascii (Span.get_name span) with
    | "tracing.flush_spans" | "tracing.file.export" | "tracing.http.export" ->
        false
    | _ ->
        true
  in
  let span = start_test_span () in
  ( match span with
  | Ok x ->
      Alcotest.(check bool)
        "Spans are collected in span hashtable"
        (Tracer.span_hashtbl_is_empty ())
        false ;

      let _ = Tracer.finish x in
      Alcotest.(check bool)
        "Span finish removes span from span hashtable"
        (Tracer.span_hashtbl_is_empty ())
        true ;
      Alcotest.(check bool)
        "Span finish adds span to finished_spans hashtable"
        (Tracer.finished_span_hashtbl_is_empty ())
        false ;

      Destination.flush_spans () ;

      (* Flushing the spans always creates two spans if there are tracer providers enabled.
         - Tracing.flush_spans;
         - Tracing.File.export/Tracing.Http.export.

         Therefore, the finished spans table is not always empty after flushing.
      *)
      let _, finished_spans = Spans.dump () in
      let filtered_spans_count =
        finished_spans
        |> fst
        |> List.to_seq
        |> Seq.filter filter_export_spans
        |> Seq.length
      in
      Alcotest.(check int)
        "Span export clears finished_spans hash table" filtered_spans_count 0
  | Error e ->
      Alcotest.failf "Span start failed with %s" (Printexc.to_string e)
  ) ;
  clear_dir ~test_trace_log_dir () ;
  test_destroy ~__context ~self () ;
  TracerProvider.assert_num_observers ~__context 0

let raise_exn () = raise (Failure "Test exception message")

let test_b () = raise_exn () + 1 (* non-tail to ensure stack entry created *)

let test_a () = test_b () + 1

let test_tracing_exn_backtraces () =
  let __context = Test_common.make_test_database () in
  let self = test_create ~__context ~enabled:true () in
  let span = start_test_span () in
  ( match span with
  | Ok x -> (
    try
      let (_ : int) = test_a () in
      ()
    with e -> (
      let stacktrace = Printexc.get_backtrace () in
      let x = Tracer.finish ~error:(e, stacktrace) x in
      match x with
      | Ok (Some span) ->
          let span_stacktrace = Span.get_tag span "exception.stacktrace" in
          debug "STACKTRACE: %s" span_stacktrace ;
          List.iter
            (fun fun_str ->
              let re = Re.Posix.compile_pat (".*" ^ fun_str ^ ".*") in
              let function_match = Re.execp re span_stacktrace in
              Alcotest.(check bool)
                (Printf.sprintf "function %s found in backtrace" fun_str)
                true function_match
            )
            ["raise_exn"; "test_b"; "test_a"]
      | Ok None ->
          Alcotest.failf "Span finish failed"
      | Error _ ->
          Alcotest.failf "Failed to fetch exception stacktrace"
    )
  )
  | Error e ->
      Alcotest.failf "Span start failed with %s" (Printexc.to_string e)
  ) ;
  test_destroy ~__context ~self ()

let test_attribute_validation () =
  let good_attributes =
    [
      ("foo", "bar")
    ; ("foo1", "bar")
    ; ("fo_o", "bar")
    ; ("xs.foo", "bar")
    ; ("foo", "")
    ; ( "wtfuphowwruztfzmreivxecuvupijqfvevswqvyxrumelhrkgzsqrrjvkqduiabhfdhhxutmrwloiauesoffowntmqmtiicyprozaivvtzencalwdnkkepcolebicdzrrudeehohgzocmplfzlqaqheyjphhbiqvmclghmjofiayhtaxwpwsahobolxubehbvojodqijszfzmawlkivgagvzmprlpiflzjtvskpthyhwrpemobxmqbpucddgxnh"
      , ""
      )
    ; ( "foo"
      , "hghlalxthfhzmxetbkycuksigldfmsnshkqobezlgltebpnijbryhicwxqhkupaxiskdfpviutbsxfoshoputqdjxghkqxlcgdmiyykdpxhzllexqnoxdpxposvmrzcwrptjhxrldcxbjzriwyvjyfkqagvnnnboqbdsblhtcxrzfabksheiynqkzaaowlktaollfgiwfgchtydbnvdzvzlbxythfhlsmgortqgdchlxfstnausluyboqsijjkoprnjbwdkkjhaaobobpfczuakhhxodjhvxazzighjmyhsnixdpinyikprohgnxfoboggenkyfbtspaznvptkgecuboddzpswomulnydfktzzxabaprupovgssoaitddxlogumiqspbuglowpkbqajjmraysnftlfvgfwhbuknhiqvvcvmxgrnviboqdaherfkgmegxcgjhfbbcaghpgppyvflyqdlgxbssosnwmhptwqpqpxkztxeagirzxqacieztcsrqspekkgefvpyymtqaxexvsdrayfrsgsadpvqvqbsfloabkuluraaeuwaqwbazzmvxfgrhepqttocslkwcrlrvbvecdfdloqvnqijonxnkjrdrrhylajiotwtrjhckislyaqdeexpvklqluawobdmmxapwzvayfmmcjcsztypnneeauuyfwhkpkxgyylwrsolurrxytogsakpvqlqvfrbsrcqgewjgbsattxffxbrdoyillsayfthcskavtbnxxuidbpzqksjkrhenzylpxwxhvdwisbadhgmmogkqhrkuivsgxvwpetvviahczvlexzsabtittcqsrqcdwldcdxaufexujbwutzydixnzdtpqumpppwzmkntfbmqpqtyhpjlclnpvbuflxwebfmsjnvkzeequxxjsrtoqfsvutiladhttufmykkitctqeliunekevntumbyitjxcxxuaoqsrnupwihhlulkzpkbdvdcfddebnvkmzpigthdhkxrouqbypxwcibjhogjbfsijhlbjprcxeeyxsdwottbqvbjvlvexgofqubhlforcsqbqkqxklrhcdlnoovsvfxrhrlwqbwdvxnkphzuqwfnvrlyamnbqwdiorhdmvdfpekuikgqxktdtgybqpzvqycbivhhllimcolxenvwiltzzahxvvvgzxxrrknnlvdyjewwugvpnpzujlujpiofbkaohjqsbndcdhbbvqzzwdlvmihejdwnztxvyrzurvrdwjixqotvkmtxladbhmfznvxebbfsaqxivdtktnbtqehyvmavpfzvybascwpaahmuskwnnkijruhqhumhoypriqhyrvjbwmwornucitwskdpexlwcbqlxttaulvqrudkzzvhwcarbeoarvaktztgnbppgzwjqakdxrzeppvabmqmqxhvpsdrzqvqsojghhxmlzcjweghhbouvuihjxhzpwscppmfptpaokfnaydofeedxxbtutjgxzntigmsrinkulhyskchtkdnkqsnoteyetkszmxwmtxudwjhyjdiznghectneaqsbaxbeybhayknvekvwqhbaqrvpcequagvwacrwyvmebsdslrsgjipsvvzqidmjpfffphyvqnpthhpgazqmbsrokrquytbyfaeqkqbkjdvzfhrdqorkqmarzmmzrvtgmuhlvxvkyvpgplwoimnpifwnmokwxvvgpmpumbiecdfczfhnhmvqsqsthnpwfyemzgwvuwqgcmzuygoouzfqzgdguwcrvzteqctvjwgszkmkgfzwjblgcjdwkqqaagiyqxfobchkngoevugothuqudcsurtdehxuwzmeghsqjrblokgllwnavjzvhykspjmioeevheajwnkrlfpjcpahedyyjugbqlyfbetaivnuxvlenrgjryhixsqmmvvuuoapqifqkrwruevdvncpeemsxqlueljntbwrxbbmmslpihfwfoqtdcucddxzkghvaxtyjtmtznffakufvgxasznqoqmaknjzzetyszgboysaazgwfsyiftwprtfugvewjzxiobvjubclobpsbsvegrykrjhbwasopmetxpptalmlbthebzvzpagqzqveetegfakofrlgpoquyyoekrogbontukuxftzgkohvqyyyhpqwinfqphyzahtdvadywkvsrzpystskcyvdytlrcvuaasbziqdscrbdfsukpyoqzqwevpfxtgpleqikcccvnfjddvwiilmpovmxzpwjrcjtagzmnytjolpoawbxpbpnzbebafirnlnaxwvgnzsgkzdtrvqnnvxrznrzegudpfbmmoxldhlpkfynkshijwuofmhzpueeoljuwgphaogfiuwycreevkhizgjxsfafkuohombprrgvfzsfiwpmbblqiqucowjbusbgaptuyoyimsrgxxorywyqsqlarphyybblakeutcebvmuifkceqvotrpnwmciftjutpvhporjsdixwcstipodmuspxeudjrosatpkwwydrbpcamlibqiscmefmyntozdgloojrsonpvtfudjfrmomtaivrazjvlyhmadnkdyynzhhisgmpyoqsbxhpnqdyrmgiwzprxezpvoojhjgwkkllfjgbwblkqjtayqnlfuumvvuyuwbuhukwnmmzpipgazfojegahbzixbriromwjjtopjfieiaakttkeuquagmviwltmvlxmerkantqhtpbxjzezcwtxbscoezpaqrssqtrlqknmkeqmqqszwyiiibxfvrdtzwopdbaxgscpsgorkgpuddehaiwotztwrcwpkbzqnighsywxccvnteeifowxzzpghbwpcgvocvyiqzfdkueqogtdnzdzxglrkfgrixpwqjqyswppkewcnafkkzqrodcfaojhqcdupxqbhwdncetgmagztxusqqpflrwxbxrnghbxwtrjvbfwnvssunhngqlxgmhqrwjrudissoibipvvqotiqyieaxzbyjuvmhvnpsuvhpcacssxxaewecwewqtkfzpqurhpgtvmcrltsmqvwokbeikuvbtzntinxrqaseheeqyzxfogexlfqlndrnucyxegblmqtubuzocgdhgndpctndrytgdhakadfamgsgpcxzijtusvdhudnrjtnsxqjkeqsplkjvuwiqngzqzkkbsyndsbdjvrwqkimzxttzdtvglofangtuggacqiqxawbwakxdtzgoabbjvsnofhxqiwuyczpypthsgehottjrhuyolsbnojkcevbrhaavupxyzjjecwqncpwhsvfyptqnytrbdqbxcrvtgfrhwvuonhxxqtwnhmiiaqzjavyggphohawxxbzcpqshdorkbkvdrmkynyrvzlfstnihnvszisttigojevrdhtrlcxmouityrhmiuclynnznrscvkantiaggbaxvegjvgnvspnziffsrjzpbtgvsoxuroezxdjksjawkxamrkmorjaoluvdlijkgqswqonutpcigjllvlxwthpwmpagvhwwpplwfmrclldneoyerymcvobopeyzsmmfptoxmqmmfllqqkdaiigctuwymsfrcrfeyarxtdkgyxjokvynatcvxylfozaqjtryeltrkzuogaoaxvzoqxibdsgzjgaqccfivjtgxrxtecqfxyckauilimzcxccudxmpffshmmivumpcrizlravdcafbiqgniydncfynslmdmtamxlqdzqupoohpdnrgemepisppgwkpgkycjkvdaojvempoeppletwnxssbwfyplhhuztmsktmvkwlcdfszwhmjgylqdisbtuhtzxxedjypgwhpkqvyqbchvxnbldqeukjkvtcnsbxpwuogkdyqwnqvmpwwnruvodbfnyhfqwlosdhzvuvqyyjhwnobfxhrccyfonyfwqubxruamisovtmfnrzsefblzoajozyblxuzcklsnirhvlunobsoaqxhyusnxayyzevgzwndrzsuyxhhccoznketmdxnxdunymekiklbpjxcnko"
      )
    ]
  in

  let bad_attributes =
    [
      ("", "bar")
    ; ("Foo", "bar")
    ; (".foo", "bar")
    ; ("foo.", "bar")
    ; ("_foo", "bar")
    ; ( "wtfuphowwruztfzmreivxecuvupijqfvevswqvyxrumelhrkgzsqrrjvkqduiabhfdhhxutmrwloiauesoffowntmqmtiicyprozaivvtzencalwdnkkepcolebicdzrrudeehohgzocmplfzlqaqheyjphhbiqvmclghmjofiayhtaxwpwsahobolxubehbvojodqijszfzmawlkivgagvzmprlpiflzjtvskpthyhwrpemobxmqbpucddgxnha"
      , ""
      )
    ; ( "foo"
      , "rovmlrouovesemihzezcgqrpvvhnkztvghscxrblrzkiazylqldirxovojdsuqcdmivhvjeszvwzozqfcgysdamukadupaagpvjrtyoaioplhmayhxgljubhfdupnqomlhnnhxyrrfnoatrrumwwbeejxndlikvqhwtdluwssyghtzzjwrzjvllvorgiuralqydlaicqhsytddurqektekezxrxzrzkwiogmspeyhjdunccaecypdzgviclszmugvvkpgcxwjsikqzwnnsuwmhswzrdkbxkosfhnkianqqdvlhejvgowutzjxmrjtwtszphylhzmwrfivqcobipavmabvijruriqmrefqopesfzlspmkizylrhjoondgjyulfohmkvmfsbcyqjfpolozfhegidwjzpgthwirxruofzevrzdwkycnssruzoxfijjghcertipbaosxjovjddlgdotavgjxqqpmrvzaravgxzsajkoxfesjunazsyehmczgcbddnayicfmxbsnyizymvhtcuwfdmugagghjmpwlgwomhwzhxonaqcdfgcgtpmxnanisldkznbixumqgxugvcrncfmceiluexjqxuimcfvpvvplrgvrbumpiyymgbqnunydyqmulbhnhlsralxmlivuzulinjpktoxedrwarnewyvisvpjngbznkcdlfqbjvymqkuakszrzctvcylqdzdwfiaybuevzabzxemdkaohackvkfwgbbeeuqfmyabgktenqdbaeawovynfcfedfzpkrrbnzuwxznrtebgdxnnvcjduibqvbywftnumbhrdanpagpouyhgscwvdylyigcmywbpaoobsvodkzqzlzpnrizhyngomwmdbouxvpddjpzglrafalbufsirtxnkcphqzhmmjpvzzulabpnlkiqexmjgwzrsglpfzerkuismwkhgllzemepxtpohszvpduhowdiutivoowrwkkqaaofltfepwcphjuvingxepfyuvcrnvdkuwaixihmdovqnwvmkxmtttyxddgnamqgdarqsrkleqvdspdpvzgqasaixsamflgoucjvpszkgjrsfevqgwegsgkdjsfraldgdepvxyxapxgwixleuztbitdtswmwchvswmrqjunfazbjrlrghvqdhrvylenhbptqioaiuvvfwwnkvhjagcxphthjznrlidxhbvtkwhgpusoujrvvhohtdqexcrghgrwydreldwmzkqmbulzmavnqzjpxorptyweecwyrlnbcarnjbztubzmpcvmptzhfaqlmgpipqjsilqrqmyhxynyjzpenjvmhiqkexqlastmipedzdkntbjxgemczfkcuvpivjixrayaxkuucergowsvyizvhmshxfytpwkutfutpstktkukktxgbwlzcpkzttaudgkuctuerteuxlmliijdszuxcmoooywyxmnxyhtlizzlpvibzrosespjubzkutaaiaqprmobsbhmeonsjqobrcnjoeivhmibgbzfhcfyndkhpdcwihzkgksintejdexxxeigyhqgkzhpxalufrxghpstdtfmkpoxzkigjthveqnsvvspygwilemjhbxgbxscikagxgxulxzydwpbbsolrrzwksitbwozcwoorbhjqjyzkgjxoinajeoflhmwuwffgtuwbanukifldswlxyakrpbvpftxhvxxcvumzlxfbfdkhvfkznvwycmwlqqjvapjnvfowzckmrwcigldhxoizjehyyhrtiabukdzujdcvetpnhnuwntcicojyscxdkfquwhpbpkzglppmgmsnukphgjxdsdkltdmutxcpezfjurcscxiwzekfrjymmwufiyerlghbkhkylvgbdrrcozjdfqiksrnueapusftvdcaneovhzqkhlastodaabklqoqfuggfglaxpttnlgoadfuiazdcwcfyhkedatgxkwzsihcfusbrodhkpflgvhjojtryhqdlmnvinwupmqohtmvraxriuhbgjatyvccorpqpzacbbxcjiahowffbsiafdimeyvsfmhyeecofwdkqkasonlzowcrjpgxxcvpvxcnzfukrfcqkclbvyulirbqexekviezidaegrmpqllmsgjjnewoyuxxqlmjoppkyrcxyqmaxpjznjpgxdgjnepaaavvlcagxecclsdjxasvgsqhwqycwuouinynoirptvhnkuctfylplehpsovtjqyjwnhicpitmcrzgejhmphjhrfhhwqsdtqrexnykjihtqlwyclapwexrqfmjimswtmngcuyriyoadgswgvwfzwzzvxnvoveklncgboaynujqkqnjvzodrgszrptfbzpobilzspvzsxkphvoibphpfwqvzfccvtxgajjvblwebjtjfntkquunebusqqqzayutzqrevjzdyskaostzsenwbyhukepxnxpexgpypwkpubmokwyazhxvalwepuqjpqmyqdykmaerkfgvxdaybwpwziyjgokmgnqbaphgfbvjdhcofoiabibzweafehdceokxmkhjqjdxgzbsnwdjrqgqghxmgpzxxodsbzttsiexbkeeghmqllvcqwfueeyvmvxumbmzpihldmkehxiaglhzsrkvuvbdzhotgpmqrczeysvjxtoivvgntjutbvstgwuvmrddlzvfnnukmscjzwscpqiuhdpmnnccktzzdpmrplyfrkoshtpfhzhfioafoqaughfqyznirdywurhrhnxxwtzwlqxyuafbotouliofepelgaemxxrhxowvfklklontnauimurflwcxscfskdubskjlokcvwcamgxhvabwumcbupfexfntlpadlwgjziflaaqjxeithotyzfwmmsmtrwgbpjbpqvxqnmozlftpeflpnychwrmcbiylhqnoceegbiahtgwvevzerafwnvxtfvzkpsjthdnidxyzsnrvgvbyozvzedgsnwgwcmetawlhjjikkulvbxlufahbzeuajmsvftradgptureuacobohuyuqqaudrghgacegwdbsvoosxkfywtaacdiicymfockrwhsooggusxxtkzeukkimssqdzrkivfqhfztlsfvmnvcbctzbwortijwwhisdgpgkxfwkshwxknwzvdcwadtlwngintslpcrfnvkoohamexlvhboasgjhcpemqgsnapeseywkhkmugfisvhdscadwovroprjgqhvnublqcbbrmdrlvplprctjvuicaqlmvqgrepxrtvblixibpzweybyyemrnpbgacqtsskxqeyzjicqpokzqnbxrnrrimchavfkbozxbmswyyborgsbdpahbeajqcgpreauwtimkylszuvmuuqyzbymzmcxwiqqtqsidgwrbepeecpbdlpniwmjourrzwzpsxypraibpmapxouwixlphhupqprllkdejsbybdqhgcjrgmmoicepibcyjtwgrgopewzpeeavxmrqphosxxfwadelhwymryugjjizggvfvukgxgfxwizdqupuhmrwnpqzyydboljgckrublpbhsimgwkbenimnflxvloqcuzmnympymtrczdgzjvetqqtlspykkpmjwxvmvsgerxalbapegpjyjnrsqujbnuwwnuvkubpihotjaeuvnuithgwzrpsmxcgqiazhaphpyriqjtryilsoqtlqzyegzrywpyllpgwmvysimnpwigdwvnkfutnsehvbbxwqptoxsnicbqqxbwocfeqwfvhefdlpfoswbrmjzhcwkdquljpelmtwyykhrjkicipyueyijicgcggftdkzqsuqpfpfonyrilunoxxzlmzhgatosrbldnsqbypdcakpzmzsugpqnfyndtrfewgxduwuxkczoewmjkeclmthlcjcxrsuyeszgwtllhccgwevuccvwvxitbsrhbmkwnuakedtqnofhviyrsjjuesybzgzlwlwdloobilntcrnltcxfjiccqgbxqzxixkqnvderfeetpwbzugooalmddmwjemzkfya"
      )
    ]
  in

  let test_good_attribute (key, value) =
    Alcotest.(check bool)
      ("Good key, value pair with " ^ key ^ ":" ^ value)
      true
      (validate_attribute (key, value))
  in

  let test_bad_attribute (key, value) =
    Alcotest.(check bool)
      ("Bad key, value pair with " ^ key ^ ":" ^ value)
      false
      (validate_attribute (key, value))
  in

  List.iter test_good_attribute good_attributes ;
  List.iter test_bad_attribute bad_attributes

let test_observed_components_of () =
  let open Xapi_globs in
  let open Xapi_observer_components in
  let original_value = !observer_experimental_components in
  let remove comp = List.filter (fun c -> comp <> c) in
  let inputs = [[]; [SMApi]; [Xapi; SMApi]; all] in

  let expected_components_given_config_value =
    [
      ( "No experimental component is expected"
      , StringSet.empty
      , [remove Xapi_clusterd all; [SMApi]; [Xapi; SMApi]; all]
      )
    ; ( "SMapi is experimental component"
      , StringSet.singleton Constants.observer_component_smapi
      , [
          all |> remove Xapi_clusterd |> remove SMApi
        ; []
        ; [Xapi]
        ; remove SMApi all
        ]
      )
    ]
  in

  let test_exp_comp (msg, v, expected_list) =
    Xapi_globs.observer_experimental_components := v ;
    let observed_components = List.map observed_components_of inputs in
    List.iter2
      (fun expected received ->
        Alcotest.(check component_set_testable)
          msg
          (ComponentSet.of_list expected)
          (ComponentSet.of_list received)
      )
      expected_list observed_components
  in

  List.iter test_exp_comp expected_components_given_config_value ;
  observer_experimental_components := original_value

module type Id = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

let testable_of_id (type a) (module I : Id with type t = a) =
  let equal a b = I.compare a b = 0 and pp = Fmt.of_to_string I.to_string in
  Alcotest.V1.testable pp equal

let trace_id = testable_of_id (module Trace_id)

let span_id = testable_of_id (module Span_id)

let test_traceid () =
  let expected = Trace_id.make () in
  let str = expected |> Trace_id.to_string in
  let actual = str |> Trace_id.of_string in
  Alcotest.V1.check' trace_id ~expected ~actual ~msg:"roundtrip" ;
  Alcotest.V1.(check' int ~expected:32 ~actual:(String.length str) ~msg:"length")

let test_traceid' () =
  let expected = "00000000000000010000000000000001" in
  let actual = expected |> Trace_id.of_string |> Trace_id.to_string in
  Alcotest.V1.(check' string ~expected ~actual ~msg:"roundtrip(str)")

let test_spanid () =
  let expected = Span_id.make () in
  let str = expected |> Span_id.to_string in
  let actual = str |> Span_id.of_string in
  Alcotest.V1.check' span_id ~expected ~actual ~msg:"roundtrip" ;
  Alcotest.V1.(check' int ~expected:16 ~actual:(String.length str) ~msg:"length")

let test_spanid' () =
  let expected = "0000000000000001" in
  let actual = expected |> Span_id.of_string |> Span_id.to_string in
  Alcotest.V1.(check' string ~expected ~actual ~msg:"roundtrip(str)")

let test =
  [
    ( "test_observer_create_and_destroy"
    , `Quick
    , test_observer_create_and_destroy
    )
  ; ("test_observer_valid_params", `Quick, test_observer_valid_params)
  ; ("test_observer_endpoint", `Quick, test_observer_endpoint)
  ; ("test_file_export", `Quick, test_file_export_writes)
  ; ("test_all_spans_finish", `Quick, test_all_spans_finish)
  ; ("test_hashtbl_leaks", `Quick, test_hashtbl_leaks)
  ; ("test_tracing_exn_backtraces", `Quick, test_tracing_exn_backtraces)
  ; ("test_attribute_validation", `Quick, test_attribute_validation)
  ; ("test_observed_components_of", `Quick, test_observed_components_of)
  ; ("test span_id", `Quick, test_spanid)
  ; ("test trace_id", `Quick, test_traceid)
  ; ("test span_id", `Quick, test_spanid')
  ; ("test trace_id", `Quick, test_traceid')
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Tracing" [("Tracing lifetime", test)]
