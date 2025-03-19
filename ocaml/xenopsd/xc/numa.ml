(* Monitoring loop that keeps track of per-numa-node memory changes, and prints
   the change. Useful to see whether memory scrubbing is seen as used or free
   memory by userspace *)
open! Xenctrlext

let ( let@ ) f x = f x

let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

let xc = get_handle ()

let binary_prefixes = [""; "Ki"; "Mi"; "Gi"; "Ti"; "Pi"]

let human_readable_bytes quantity =
  let unit = "Bs" in
  let print prefix q = Printf.sprintf "%Ld %s%s" q prefix unit in
  let rec loop acc q = function
    | [] ->
        acc
    | pre :: prefs ->
        let quotient = Int64.div q 1024L in
        let modulus = Int64.rem q 1024L in
        let acc =
          if Int64.equal modulus 0L then acc else print pre modulus :: acc
        in
        loop acc quotient prefs
  in
  if quantity = 0L then
    print "" 0L
  else
    loop [] quantity binary_prefixes |> String.concat ", "

let get_memory () =
  let {memory; _} = numainfo xc in
  memory

let print_mem c mem =
  for i = 0 to Array.length mem - 1 do
    let {memfree; memsize} = mem.(i) in
    let memfree = human_readable_bytes memfree in
    let memsize = human_readable_bytes memsize in
    Logs.app (fun m ->
        m "\t%d: %s free out of %s" i memfree memsize ~tags:(stamp c)
    )
  done

let print_diff_mem before after =
  if before > after then
    Printf.sprintf "%s 🢆 " (Int64.sub before after |> human_readable_bytes)
  else
    Printf.sprintf "%s 🢅 " (Int64.sub after before |> human_readable_bytes)

let diff c old cur =
  let changed_yet = ref false in
  for i = 0 to Int.min (Array.length old) (Array.length cur) - 1 do
    let {memfree= a_free; _}, {memfree= b_free; _} = (old.(i), cur.(i)) in
    if a_free <> b_free then (
      if not !changed_yet then
        changed_yet := true ;
      let free = human_readable_bytes b_free in
      let updown = print_diff_mem a_free b_free in
      Logs.app (fun m ->
          m "\t%d: %s free (%s)" i free updown ~tags:(stamp (c ()))
      )
    )
  done ;
  !changed_yet

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over () ; k () in
    let with_stamp h tags k ppf fmt =
      let stamp =
        match tags with
        | None ->
            None
        | Some tags ->
            Logs.Tag.find stamp_tag tags
      in
      let span_pp s =
        match s with
        | None ->
            "0ns"
        | Some s ->
            Fmt.to_to_string Mtime.Span.pp s
      in
      Format.kfprintf k ppf
        ("%a[%s] @[" ^^ fmt ^^ "@]@.")
        Logs.pp_header (level, h) (span_pp stamp)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  {Logs.report}

let memory_changes () =
  let max_time = Mtime.Span.(7 * s) in

  let memory = get_memory () in
  let c = Mtime_clock.counter () in
  print_mem c memory ;
  let rec loop since_started since_changed previous =
    let current = get_memory () in

    let since_started = ref since_started in
    let timer () =
      let last_changed = Mtime_clock.count since_changed in
      if Mtime.Span.is_longer last_changed ~than:max_time then
        since_started := Mtime_clock.counter () ;
      !since_started
    in

    let changed = diff timer previous current in

    let since_changed =
      if changed then
        Mtime_clock.counter ()
      else
        !since_started
    in
    Unix.sleepf 0.01 ;
    loop !since_started since_changed current
  in
  loop c c memory

module DomainSet = Set.Make (Int)

let get_domains xc =
  Xenctrl.domain_getinfolist xc 0
  |> List.to_seq
  |> Seq.map (function Xenctrl.{domid; _} -> domid)
  |> DomainSet.of_seq

let diff_domains c previous current =
  let added = DomainSet.diff current previous in
  let removed = DomainSet.diff previous current in
  DomainSet.iter
    (fun id -> Logs.app (fun m -> m "domain %d added" id ~tags:(stamp c)))
    added ;
  DomainSet.iter
    (fun id -> Logs.app (fun m -> m "domain %d removed" id ~tags:(stamp c)))
    removed

let domain_changes xc =
  let domains = get_domains xc in
  let c = Mtime_clock.counter () in
  let rec loop previous =
    let current = get_domains xc in
    diff_domains c previous current ;
    Unix.sleepf 0.01 ;
    loop current
  in
  loop domains

let () =
  Logs.set_reporter (reporter Format.std_formatter) ;
  Logs.set_level (Some Logs.Info) ;

  ignore (Thread.create memory_changes () : Thread.t) ;
  let@ xc = Xenctrl.with_intf in
  domain_changes xc
