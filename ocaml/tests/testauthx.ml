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
open Auth_signature
open Authx

let usage() =
  print_endline "Usage:";
  Printf.printf "%s <username> <password>\n" Sys.argv.(0);
  exit 1

let _ =

  if Array.length Sys.argv <> 3 then usage ();
  let username = Sys.argv.(1)
  and password = Sys.argv.(2) in

  let hr x = print_endline ("-----------------------------\n"^x) in

  (* should return 2037 *)
  hr ("TEST 1a. Authx.get_subject_identifier "^username);
  let userid = AuthX.methods.get_subject_identifier username in
  print_endline ("userid="^userid);


  hr ("TEST 1b. AuthX.methods.get_subject_identifier "^username^"_werq (unknown subject)");
  try
    print_endline (AuthX.methods.get_subject_identifier (username^"_werq"));
  with Not_found -> print_endline "subject Not_found, as expected";


    (* should return a list of groups that subjectid 1000 (a user) belongs to *)
    hr ("TEST 2a. AuthX.methods.query_group_membership "^userid^" (a user subject)");
    let conc x y = (x^","^y) in
    let groupid_list = AuthX.methods.query_group_membership userid in
    print_endline (List.fold_left (conc) "" groupid_list);

    (* should return a list of groups that subjectid 10024 (a group) belongs to *)
    let agroup = List.hd groupid_list in
    hr ("TEST 2b. AuthX.methods.query_group_membership "^agroup^" (a group subject)");
    print_endline (List.fold_left (conc) "" (AuthX.methods.query_group_membership agroup));


    hr "TEST 2c. AuthX.methods.query_group_membership u999 (unknown subject)";
    try
      print_endline (List.fold_left (conc) "" (AuthX.methods.query_group_membership "u999"));
    with Not_found -> print_endline "subject Not_found, as expected.";

      hr "TEST 2d. AuthX.methods.query_group_membership a999 (unknown subject)";
      try
        print_endline (List.fold_left (conc) "" (AuthX.methods.query_group_membership "a999"));
      with Not_found -> print_endline "subject Not_found, as expected.";

        hr "TEST 2e. AuthX.methods.query_group_membership 999 (unknown subject)";
        try
          print_endline (List.fold_left (conc) "" (AuthX.methods.query_group_membership "999"));
        with Not_found -> print_endline "subject Not_found, as expected.";

          (* should return a list with information about subject_id 1000 (a user)*)
          hr ("TEST 3a. AuthX.methods.query_subject_information "^userid^" (a user)");
          let infolist1 = AuthX.methods.query_subject_information userid in
          for i=0 to (List.length infolist1)-1 do
            let print_elems (e1,e2) = print_endline (e1^": "^e2) in
            print_elems (List.nth infolist1 i)
          done;

          (* should return a list with information about subject_id 10024 (a group)*)
          hr ("TEST 3b. AuthX.methods.query_subject_information "^agroup^" (a group)");
          let infolist1 = AuthX.methods.query_subject_information agroup in
          for i=0 to (List.length infolist1)-1 do
            let print_elems (e1,e2) = print_endline (e1^": "^e2) in
            print_elems (List.nth infolist1 i)
          done;

          (* should return an error not_found *)
          hr "TEST 3c. AuthX.methods.query_subject_information u999 (unknown subject)";
          try
            let infolist1 = AuthX.methods.query_subject_information "u999" in
            for i=0 to (List.length infolist1)-1 do
              let print_elems (e1,e2) = print_endline (e1^": "^e2) in
              print_elems (List.nth infolist1 i)
            done;
          with Not_found -> print_endline "subject Not_found, as expected.";

            (* should return an error not_found *)
            hr "TEST 3d. AuthX.methods.query_subject_information a999 (unknown subject)";
            try
              let infolist1 = AuthX.methods.query_subject_information "a999" in
              for i=0 to (List.length infolist1)-1 do
                let print_elems (e1,e2) = print_endline (e1^": "^e2) in
                print_elems (List.nth infolist1 i)
              done;
            with Not_found -> print_endline "subject Not_found, as expected.";

              (* should return an error not_found *)
              hr "TEST 3e. AuthX.methods.query_subject_information 999 (unknown subject)";
              try
                let infolist1 = AuthX.methods.query_subject_information "999" in
                for i=0 to (List.length infolist1)-1 do
                  let print_elems (e1,e2) = print_endline (e1^": "^e2) in
                  print_elems (List.nth infolist1 i)
                done;
              with Not_found -> print_endline "subject Not_found, as expected.";

                hr ("TEST 4. AuthX.methods.authenticate_username_password "^username);
                print_endline (AuthX.methods.authenticate_username_password username password);



