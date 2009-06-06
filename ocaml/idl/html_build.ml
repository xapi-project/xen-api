(* Merge two trees of HTML fragments together into one, create navigation aids *)


open Printf
open Stringext
open Listext

open Html_common


(** A tree of resources *)
type tree = 
  | Leaf
  | Node of (string * tree) list

let chop x = try Filename.chop_extension x with _ -> x
let is_dir x = try (Unix.stat x).Unix.st_kind = Unix.S_DIR with _ -> false 

(** Reads a tree from disk. Each file has its extension chopped: the intention is to 
    merge these files together into one single page *)
let rec tree_from_disk (path: string) = 
  let ls x = List.filter (fun x -> not(String.startswith "." x)) (Array.to_list (Sys.readdir x)) in
  if is_dir path
  then Node(List.map (fun x -> (chop x), tree_from_disk (Filename.concat path x)) (ls path))
  else Leaf

let rec merge (a: tree) (b: tree) = match a, b with
  | Leaf, Leaf -> Leaf
  | Node a, Node b -> 
      let keys = List.setify (List.map fst (a @ b)) in
      Node(List.map (fun key -> key, (match List.mem_assoc key a, List.mem_assoc key b with
				      | true, true -> merge (List.assoc key a) (List.assoc key b)
				      | true, false -> List.assoc key a
				      | false, true -> List.assoc key b
				      | false, false -> failwith "tree merge internal")) keys)
  | _, _ -> failwith "tree merge cannot merge a Leaf with a Node"

let output sources dest (a: tree) = 

  let rec dots x = if x = 0 then "" else "../" ^ (dots (x-1)) in

  let findall path name = 
    let tosearch = List.map (fun x -> Filename.concat x path) sources in
    let one dir = 
      try
	let all = List.filter (fun x -> chop x = name) (Array.to_list (Sys.readdir dir)) in
	let all = List.map (Filename.concat dir) all in
	List.filter (fun x -> not(is_dir x)) all
      with _ -> [] in
    List.concat (List.map one tosearch) in

  let merge path files = 
    (* Should order the files. Autogen first obviously *)
    let ordered = 
      if List.exists (String.endswith autogen) files
      then 
	let auto = List.find (String.endswith autogen) files 
	and rest = List.sort compare (List.filter (fun x -> not(String.endswith autogen x)) files) in
	auto :: rest
      else files in

    let breadcrumbs = 
      let us = Filename.basename (chop (List.hd files) ^ ".html") in
      let parts = us :: (List.rev (String.split '/' path)) in
      let parts = List.filter (fun x -> x <> "") parts in
      let links = List.mapi (fun idx dir -> 
			       if dir = _Classes
			       then dots (idx + 1) ^ "classes.html"
			       else dots idx ^ dir) parts in
      let links = List.filter (fun (label, _) -> not(List.mem label [ _Implicit; _Explicit; _Fields ])) (List.combine parts links) in
      let hrefs = List.map (fun (label, link) ->
			      href link [ Xml.PCData label ]) links in

      (* add a between / *)
      div "breadcrumbs" (List.rev (List.tl (List.concat (List.map (fun x -> [ Xml.PCData "/"; x ]) hrefs)))) in
    let parse x = 
      try
	Xml.parse_file x
      with Xml.Error e ->
	failwith (Printf.sprintf "Caught XML error parsing %s: %s" x (Xml.error e)) in

    let main = List.map parse ordered in
    let rel = dots (List.length (List.filter (fun x -> x <> "") (String.split '/' path)) + 1) in
    html ~rel ~hdr:false path 
      [ div "main"
	  (breadcrumbs :: main) ] in

  let rec walk path = function
    | Leaf -> ()
    | Node [] -> ()
    | Node ((name, Leaf) :: rest) ->
	(* Find resources, build *)
	let resources = findall path name in
	let merged = merge path resources in
	write_html merged (dest ^ "/" ^ path ^ "/" ^ name ^ ".html");
	walk path (Node rest)
    | Node ((name, subtree) :: rest) ->
	walk (Filename.concat path name) subtree;
	walk path (Node rest) in

  walk "/" a

let images_file = "images/file.gif"
let images_folder = "images/folder.gif"

let nested_uls_of_tree t = 
  let href path label = 
    Xml.Element("a", [ "href", String.concat "/" (List.rev ((label ^ ".html") :: path)); "target", "detail" ],
	    [ Xml.PCData label ]) in
  let rec tree path label = function
    | Leaf -> li [ img images_file; href path label ]
    | Node children -> li [ img images_folder; Xml.PCData label; ul (List.map (child (label :: path)) children) ]
  and child path (label, t) = tree path label t in
  match t with
  | Node children -> ul (List.map (child ["API"]) children)
  | Leaf -> failwith "Bizarrely shaped tree"
let html_of_tree t = 
  let html_tree = Xml.Element("ul", [ "id", "browser"; "class", "dir" ], 
			  [ li [ img images_folder; 
				 Xml.PCData "API";
				 nested_uls_of_tree t ]
			  ]) in

  html ~hdr:false ~css:[ "apitreeview.css" ] ~scripts:[ "apitreeview.js" ] "API" [ html_tree ]

	       
let _ = 
  let autogen = ref ""
  and manual = ref ""
  and output_dir = ref "" 
  and tree = ref "" in
    
   Arg.parse [ "-autogen", Arg.Set_string autogen, "path to the autogen files";
	       "-manual", Arg.Set_string manual, "path to the manually-created files";
	       "-output", Arg.Set_string output_dir, "path to the output dir";
	       "-tree", Arg.Set_string tree, "path to navigator tree";
	     ]
     (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
     "build API HTML reference docs";
  if !autogen = ""
  then failwith "Must set an autogen directory";
  if !manual = ""
  then failwith "Must set a manual directory";
  if !output_dir = ""
  then failwith "Must set an output directory";

  let a = tree_from_disk !autogen in
  let b = tree_from_disk !manual in
  let all = merge a b in
  output [ !autogen; !manual ] !output_dir all;

  if !tree <> ""
  then write_html (html_of_tree all) !tree;

  print_endline "done"


