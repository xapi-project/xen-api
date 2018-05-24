(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Db_rpc_common_v1
open Db_cache_types
open Db_filter
open Db_filter_types

(* Check, for randomly chosen x's, that (unmarshall (marshall x)) = x *)

(* good enough for unit test.. *)
let _ = Random.init (int_of_float (Unix.time()))

let gen_random_string() =
  let len = Random.int 20 in
  let ranchar() = Char.chr ((Random.int 90)+32) in
  let string = Bytes.create len in
  let rec fillstr l =
    if l=len then () else
      begin
        Bytes.set string l (ranchar());
        fillstr (l+1)
      end in
  fillstr 0;
  Bytes.unsafe_to_string string

let gen_random_string_option() =
  if (Random.int 2)=0 then
    None
  else
    Some (gen_random_string())

let gen_random_bool() =
  (Random.int 2)=0

let gen_random_list f =
  let len = Random.int 50 in
  let rec makesl x =
    match x with
      0 -> []
    | n -> (f())::(makesl (n-1)) in
  makesl len

let gen_random_where () =
  {table=gen_random_string();
   return=gen_random_string();
   where_field=gen_random_string();
   where_value=gen_random_string()}

exception RandomRangeError
let gen_random_structured_op () =
  match (Random.int 4) with
    0 -> AddSet
  | 1 -> RemoveSet
  | 2 -> AddMap
  | 3 -> RemoveMap
  | _ -> raise RandomRangeError (* should never be thrown *)

let gen_random_2string() =
  (gen_random_string(), gen_random_string())

let gen_random_3string() =
  (gen_random_string(), gen_random_string(), gen_random_string())

let gen_random_4string() =
  (gen_random_string(), gen_random_string(), gen_random_string(), gen_random_string())


(* test marshall unmarshall is id *)
let tm u m x =
  let s = m x in
  print_string (Xml.to_string_fmt s);
  print_string "\n\n";
  (u s)=x

let test_gtfr_args() =
  tm
    unmarshall_get_table_from_ref_args
    marshall_get_table_from_ref_args
    (gen_random_string())

let test_gtfr_response() =
  tm
    unmarshall_get_table_from_ref_response
    marshall_get_table_from_ref_response
    (gen_random_string_option())

let test_ivr_args() =
  tm
    unmarshall_is_valid_ref_args
    marshall_is_valid_ref_args
    (gen_random_string())

let test_ivr_response() =
  tm
    unmarshall_is_valid_ref_response
    marshall_is_valid_ref_response
    (gen_random_bool())

let test_rr_args() =
  tm
    unmarshall_read_refs_args
    marshall_read_refs_args
    (gen_random_string())

let test_rr_response() =
  tm
    unmarshall_read_refs_response
    marshall_read_refs_response
    (gen_random_list gen_random_string)

let test_rfw_args() =
  tm
    unmarshall_read_field_where_args
    marshall_read_field_where_args
    (gen_random_where())

let test_cra_args() =
  tm
    unmarshall_create_row_args
    marshall_create_row_args
    (gen_random_string(),
     gen_random_list gen_random_2string,
     gen_random_string())

let test_cra_response() =
  tm
    unmarshall_create_row_response
    marshall_create_row_response
    ()

let test_dr_args() =
  tm
    unmarshall_delete_row_args
    marshall_delete_row_args
    (gen_random_2string())

let test_dr_response() =
  tm
    unmarshall_delete_row_response
    marshall_delete_row_response
    ()

let test_wf_args() =
  tm
    unmarshall_write_field_args
    marshall_write_field_args
    (gen_random_4string())

let test_wf_response() =
  tm
    unmarshall_write_field_response
    marshall_write_field_response
    ()

let test_rf_args() =
  tm
    unmarshall_read_field_args
    marshall_read_field_args
    (gen_random_3string())

let test_rf_response() =
  tm
    unmarshall_read_field_response
    marshall_read_field_response
    (gen_random_string())

let test_psf_args() =
  tm
    unmarshall_process_structured_field_args
    marshall_process_structured_field_args
    (gen_random_2string(),
     gen_random_string(),
     gen_random_string(),
     gen_random_string(),
     gen_random_structured_op())

let test_psf_response() =
  tm
    unmarshall_process_structured_field_response
    marshall_process_structured_field_response
    ()

let test_readrec_args() =
  tm
    unmarshall_read_record_args
    marshall_read_record_args
    (gen_random_string(),
     gen_random_string())

let test_readrec_response() =
  tm
    unmarshall_read_record_response
    marshall_read_record_response
    (gen_random_list gen_random_2string,
     gen_random_list (fun ()->(gen_random_string(), gen_random_list gen_random_string)))

let test_exp =
  And(Eq(Field "asd", Literal "qwe"),
      Or(Eq(Field "asd", Literal "qwe"),
         And(Eq(Field "asd", Literal "qwe"),
             Not False)))

let test_find_refs_with_filter_args() =
  tm
    unmarshall_find_refs_with_filter_args
    marshall_find_refs_with_filter_args
    (gen_random_string(),
     test_exp)

let tests =
  [
    test_gtfr_args, "test_gtfr_args";
    test_gtfr_response, "test_gtfr_response";
    test_ivr_args, "test_ivr_args";
    test_ivr_response, "test_ivr_response";
    test_rr_args, "test_rr_args";
    test_rr_response, "test_rr_response";
    test_cra_args, "test_cra_args";
    test_cra_response, "test_cra_response";
    test_dr_args, "test_dr_args";
    test_dr_response, "test_dr_response";
    test_wf_args, "test_wf_args";
    test_wf_response, "test_wf_response";
    test_rf_args, "test_rf_args";
    test_rf_response, "test_rf_response";
    test_psf_args, "test_psf_args";
    test_psf_response, "test_psf_response";
    test_readrec_args, "test_readrec_args";
    test_readrec_response, "test_readrec_response";
    test_find_refs_with_filter_args, "test_find_refs_with_filter_args"
  ]

(*
let x =
  expr_of_xml (xml_of_expr test_exp)

let _ =
  print_string (string_of_expr x)
*)

exception TestFail of string

let _ =
  List.iter
    (fun (f,fname) ->
       if not (f()) then raise (TestFail fname))
    tests
