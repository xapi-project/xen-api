(* Useful helper fns *)
let quote s = Printf.sprintf "\"%s\"" s 
let o = fun f g x -> f (g x) 

type absty = | AInt of int64
	     | AStr of string
	     | AStruct of (string * absty) list
	     | AArr of absty list
		 
let rec fieldstr f =
  match f with 
    | AInt x -> Printf.sprintf "%Ld" x
    | AStr x -> Printf.sprintf "'%s'" x
    | AStruct x -> "{\n" ^ (String.concat "," (List.map (fun (x,y) -> Printf.sprintf "%s: %s\n" x (fieldstr y)) x)) ^ "}\n"
    | AArr x -> "[" ^ (String.concat "," (List.map fieldstr x)) ^ "]"

let expect_string name field =
  match field with 
    | AStr s -> s
    | AInt n -> Printf.sprintf "%Ld" n
    | _ -> failwith (Printf.sprintf "Expecting string for identifier '%s'" name)

let expect_int name field =
  match field with 
    | AInt n -> n
    | _ -> failwith (Printf.sprintf "Expecting string for identifier '%s'" name)

let expect_struct name field =
  match field with
    | AStruct fields -> fields
    | _ -> failwith (Printf.sprintf "Expecting struct for identifier '%s'" name)

let expect_array name field =
  match field with
    | AArr a -> a
    | _ -> failwith (Printf.sprintf "Expecting array for identifier '%s'" name)
     
let expect_mapped_field transform name alist =
  let field = List.assoc name alist in
  transform name field
	
let expect_mapped_string = expect_mapped_field expect_string 
let expect_mapped_int = expect_mapped_field expect_int
let expect_mapped_struct = expect_mapped_field expect_struct
let expect_mapped_array = expect_mapped_field expect_array

let map_expected_mapped_array name fn alist =
  let a = expect_mapped_array name alist in 
  List.map fn a

let filter_structs alist = 
  List.filter (fun (a,b) -> match b with | AStruct _ -> true | _ -> false) alist




  
