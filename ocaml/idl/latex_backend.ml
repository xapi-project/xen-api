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
open Printf

open Datamodel_types
open Datamodel
open Datamodel_utils
open Dm_api

open Xstringext

let rec formatted_wrap formatter s = 
  let split_in_2 c s =
    match String.split ~limit:2 c s with
        h :: t -> (h, if t = [] then "" else List.hd t)
      | [] -> assert false
  in
  let prespace, postspace = split_in_2 ' ' s in
  let preeol, posteol = split_in_2 '\n' s in

    if String.length prespace < String.length preeol then
      (Format.fprintf formatter "%s@ " prespace;
       if String.length postspace > 0 then
         formatted_wrap formatter postspace)
    else
      (if String.length posteol > 0 then
         (Format.fprintf formatter "%s@\n" preeol;
          formatted_wrap formatter posteol)
       else
         Format.fprintf formatter "%s@ " preeol)


let wrap s =
  let buf = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buf in

    Format.pp_open_hvbox formatter 0;
    Format.pp_set_margin formatter 76;
    formatted_wrap formatter s;
    Format.pp_close_box formatter ();

    Format.fprintf formatter "%!";

    Buffer.contents buf

let escape s =
  let sl = String.explode s in
  let in_quote = ref true in
  let esc_char =
    function
        '"' -> in_quote := not !in_quote; if !in_quote then "''" else "``"
      | '_' -> "\\_"
      | '#' -> "\\#"
      | c -> String.make 1 c in
  let escaped_list = List.map esc_char sl in
    String.concat "" escaped_list

let full_stop s =
  if String.length s = 0 then s^"."
  else
    if String.get s (String.length s - 1) != '.'
    then
      s ^ "."
    else
      s

let rtrim = String.rtrim

let vgap = "\n\\vspace{1cm}\n"
let hgap = "\\hspace{0.5cm}"

let is_prim_type = function
  | String | Int | Float | Bool | DateTime -> true
  | _ -> false

let is_prim_opt_type = function
  | None -> true
  | (Some (ty,_)) -> is_prim_type ty

let rec of_ty_verbatim = function
  | String -> "string" | Int -> "int" | Float -> "float" | Bool -> "bool"
  | DateTime -> "datetime" | Enum (name, things) -> name
  | Set x ->
      if (is_prim_type x) then
	(of_ty_verbatim x) ^ " Set"
      else "("^(of_ty_verbatim x)^") Set"
  | Map (a, b) -> "(" ^ (of_ty_verbatim a) ^ " -> " ^ (of_ty_verbatim b) ^ ") Map"
  | Ref obj -> obj ^ " ref"
  | Record obj -> obj ^ " record"


let rec of_ty = function
  | String -> "string" | Int -> "int" | Float -> "float" | Bool -> "bool"
  | DateTime -> "datetime" | Enum (name, things) -> escape name
  | Set x ->
      if (is_prim_type x) then
	(of_ty x) ^ " Set"
      else "("^(of_ty x)^") Set"
  | Map (a, b) -> "(" ^ (of_ty a) ^ " $\\rightarrow$ " ^ (of_ty b) ^ ") Map"
  | Ref obj -> (escape obj)^" ref"
  | Record obj -> escape obj ^ " record"

let of_ty_opt = function
    None -> "void" | Some(ty, _) -> of_ty ty

let of_ty_opt_verbatim = function
    None -> "void" | Some(ty, _) -> of_ty_verbatim ty

let desc_of_ty_opt = function
    None -> "" | Some(_, desc) -> desc

(** Add namespaces (separated by /) to each field name *)
let flatten stuff =
  let rec f ns = function
    | Field fr -> Field { fr with field_name = ns ^ fr.field_name} 
    | Namespace(ns', contents) -> Namespace("", List.map (f (ns ^ ns' ^ "/")) contents)
  in
    f "" stuff

let string_of_qualifier = function
  | StaticRO   -> "$\\mathit{RO}_\\mathit{ins}$" 
  | DynamicRO  -> "$\\mathit{RO}_\\mathit{run}$"
  | RW         -> "$\\mathit{RW}$"


let string_of_open_product release =
  let xe = if release.opensource = [] then "XE" else "" in
  let dep = if release.internal_deprecated_since = None then "" else "(deprecated)" in
  Printf.sprintf "%s %s &" xe dep

let of_enum_alias name options = [
				   "\\begin{longtable}{|ll|}";
				   "\\hline";
				   "{\\tt enum " ^ (escape name) ^ "} & \\\\";
				   "\\hline" ] @ 
  (List.map (fun (option, description) ->
	       hgap ^ "{\\tt " ^ (escape option) ^ "} & " ^ (escape description) ^ " \\\\") options) @
				   [
				     "\\hline";
				     "\\end{longtable}"
				   ]
				   

let of_content x closed = 
  let rec f prefix = function
    | Field{release=release; qualifier=qualifier; field_name=name; ty=ty; field_description=description} -> 
	[ sprintf "%s%s & %s {\\tt %s} & %s & %s \\\\" 
	    (if closed then
               string_of_open_product release
             else
               "")
            (string_of_qualifier qualifier)
            prefix (escape name) (of_ty ty) (escape description) ]
    | Namespace(_, fields) -> List.concat (List.map (f prefix) fields)
  in f "" x
    
(*
  let header = [ "\\documentclass[8pt]{article}"; 
  "\\usepackage{geometry}";
  "\\usepackage{layout}";
  "\\geometry{";
  "\left=2.0cm,";
  "\right=2.5cm,";
  "\top=3.5cm,";
  "\bottom=3cm";
  "}";
  "\\usepackage{graphics}";
  "\\usepackage{longtable}";
(* "\\usepackage[a3paper,pdftex]{geometry}"; *)
  "\\usepackage{fancyhdr}";
  "\\setlength\\parindent{0pt}";
  "\\begin{document}" ]
  let footer = [ "\\end{document}" ]
*)

    
(* Output API parameter table entry *)
let mk_latex_param p =
  String.concat " "
    ["{\\tt"; of_ty p.param_type; "}"; "&";
     escape p.param_name; "&";
     escape p.param_doc; "\\\\ \\hline"; "\n"]

let mk_latex_error err =
  sprintf "{\\tt %s}" (escape err.err_name)

let space = "\\vspace{0.3cm}"
  
(* Make a latex section for an API-specified message *)
let latex_section_of_message closed section_prefix x =
  String.concat "\n"
    ([
       String.concat "" ["\\"^section_prefix^"subsection{RPC name:~"; escape x.msg_name; "}\n"];
       "{\\bf Overview:} ";
       if x.msg_release.internal_deprecated_since <> None
       then "{\\bf This message is deprecated}"
       else "";
       wrap (full_stop (escape x.msg_doc));
       " \\noindent {\\bf Signature:} ";

       let section_contents = 
	   (String.concat " "
	      [if is_prim_opt_type x.msg_result then of_ty_opt_verbatim x.msg_result
	      else "("^(of_ty_opt_verbatim x.msg_result)^")";
	      x.msg_name;
	      String.concat ""
		[
		  "(";	 
		  String.concat ", "
		    ((if x.msg_session then ["session_id s"] else []) @
		       (List.map (fun p -> of_ty_verbatim p.param_type ^ " " ^ p.param_name) x.msg_params));
		  ")"
		]
	      ]) in
       String.concat ""
	 (if closed then
           ["\n\n{\\parbox{ \\columnwidth }{\\tt ~~~~~~~";
            escape section_contents;
	    "}}\n\n"]
         else
           ["\\begin{verbatim} ";
            section_contents;
	    "\\end{verbatim}\n\n"])
     ] @
       
       (if x.msg_params=[] then []
	else
	    [
	      "\\noindent{\\bf Arguments:}\n\n ";
	      space;
	      
	      "\\begin{tabular}{|c|c|p{7cm}|}\n \\hline";
	      "{\\bf type} & {\\bf name} & {\\bf description} \\\\ \\hline";
	      String.concat "\n" ((List.map mk_latex_param) x.msg_params);
	      "\\end{tabular}\n";
	    ]) @

       [
	 space;
	 "\n \\noindent {\\bf Return Type:} ";      
	 "{\\tt ";
	 of_ty_opt x.msg_result; "}\n\n";
	 escape (desc_of_ty_opt x.msg_result);
         space
       ] @

       (if x.msg_errors=[] then [space; space]
	else
	    [
              "";
              wrap (sprintf "\\noindent{\\bf Possible Error Codes:} %s"
	              (String.concat ", " ((List.map mk_latex_error)
                                             x.msg_errors)));
              "\\vspace{0.6cm}"
	    ]))
    
(* Make a load of sections for a list of functions, fb.
   if section_prefix="" then we make subsections for each function.
   if section_prefix="sub" then we make subsubsections for each function. *)

let latex_of_funblock closed section_prefix fb =
  String.concat "\n" (List.map (latex_section_of_message closed section_prefix) fb)


(**
 * The header for the table containing the fields of the given class.  This
 * table has an additional column if closed is true.
 *)
let class_header x closed =
  if closed then
    [
      "\\begin{longtable}{|lllp{0.2\\textwidth}p{0.38\\textwidth}|}";
      "\\hline";
      "\\multicolumn{2}{|l}{Name} & \\multicolumn{3}{l|}{\\bf " ^ (escape x.name) ^ "} \\\\";
      "\\multicolumn{2}{|l}{Description} & \\multicolumn{3}{l|}{\\parbox{11cm}{\\em " ^ (rtrim (wrap (full_stop (escape x.description)))) ^ "}} \\\\";
      "\\hline";
      " & Quals & Field & Type & Description \\\\";
      "\\hline"
    ]
  else
    [
      "\\begin{longtable}{|lllp{0.38\\textwidth}|}";
      "\\hline";
      "\\multicolumn{1}{|l}{Name} & \\multicolumn{3}{l|}{\\bf " ^ (escape x.name) ^ "} \\\\";
      (rtrim (wrap ("\\multicolumn{1}{|l}{Description} & \\multicolumn{3}{l|}{\\parbox{11cm}{\\em " ^ (escape (full_stop x.description))))) ^ "}} \\\\";
      "\\hline";
      "Quals & Field & Type & Description \\\\";
      "\\hline"
    ]


let class_footer =    
  [
    "\\hline";
    "\\end{longtable}"
  ]


let field_table_of_obj newpage x closed =
  let field_tex = List.concat (List.map (fun x -> of_content (flatten x) closed) x.contents) in
    (if newpage then ["\\newpage"] else []) @
    [
      "\\subsection{Fields for class: "^(escape x.name)^"}";
    ] @
      (if x.contents=[] then
	   ["{\\bf Class "^(escape x.name)^" has no fields.}"]
       else
           (class_header x closed) @ field_tex @ class_footer)  
    
let of_obj x closed =
  [ 
    "\\newpage";
    "\\section{Class: "^(escape x.name)^"}" ]
  @ (field_table_of_obj false x closed)
  @
      [
	"\\subsection{RPCs associated with class: "^(escape x.name)^"}"
      ]
    @
      (if x.messages=[] then
	   ["\n\n";
	    "{\\bf Class "^(escape x.name)^" has no additional RPCs associated with it.}"]
       else
	   [
	     latex_of_funblock closed "sub" x.messages
	   ])

let error_signature name params =
  if params = [] then
"
\\vspace{0.3cm}
No parameters."
  else
    sprintf "
\\vspace{0.3cm}
{\\bf Signature:}
\\begin{verbatim}%s(%s)\\end{verbatim}"
      name (String.concat ", " params)

let error_doc { err_name=name; err_params=params; err_doc=doc } =
  printf "
\\subsubsection{%s}

%s%s
\\begin{center}\\rule{10em}{0.1pt}\\end{center}
" (escape name) (wrap (escape doc)) (error_signature name params)

let include_file ?(escaped=false) ?(blanklines=false) filename = 
  let ic = open_in filename in
    try
      while true do
	let line = input_line ic in
	  print_endline (if escaped then escape line else line);
	  if blanklines then print_endline "";
      done
    with End_of_file -> ()

and error_section () =
    print_endline "\\newpage
\\section{Error Handling}
When a low-level transport error occurs, or a request is malformed at the HTTP
or XML-RPC level, the server may send an XML-RPC Fault response, or the client
may simulate the same.  The client must be prepared to handle these errors,
though they may be treated as fatal.  On the wire, these are transmitted in a
form similar to this:

\\begin{verbatim}
    <methodResponse>
      <fault>
        <value>
          <struct>
            <member>
                <name>faultCode</name>
                <value><int>-1</int></value>
              </member>
              <member>
                <name>faultString</name>
                <value><string>Malformed request</string></value>
            </member>
          </struct>
        </value>
      </fault>
    </methodResponse>
\\end{verbatim}

All other failures are reported with a more structured error response, to
allow better automatic response to failures, proper internationalisation of
any error message, and easier debugging.  On the wire, these are transmitted
like this:

\\begin{verbatim}
    <struct>
      <member>
        <name>Status</name>
        <value>Failure</value>
      </member>
      <member>
        <name>ErrorDescription</name>
        <value>
          <array>
            <data>
              <value>MAP_DUPLICATE_KEY</value>
              <value>Customer</value>
              <value>eSpeil Inc.</value>
              <value>eSpeil Incorporated</value>
            </data>
          </array>
        </value>
      </member>
    </struct>
\\end{verbatim}

Note that {\\tt ErrorDescription} value is an array of string values. The
first element of the array is an error code; the remainder of the array are
strings representing error parameters relating to that code.  In this case,
the client has attempted to add the mapping {\\tt Customer $\\rightarrow$
eSpiel Incorporated} to a Map, but it already contains the mapping
{\\tt Customer $\\rightarrow$ eSpiel Inc.}, and so the request has failed.

Each possible error code is documented in the following section.

\\subsection{Error Codes}";

  (* Sort the errors alphabetically, then generate one section per code. *)
  let errs =
    Hashtbl.fold (fun name err acc -> (name, err) :: acc)
      Datamodel.errors []
  in
    List.iter error_doc
      (snd (List.split
              (List.sort (fun (n1, _) (n2, _)-> compare n1 n2) errs)))

let first_sentence s =
  List.hd (String.split '.' s)

let all api closed =
  (* Remove private messages that are only used internally (e.g. get_record_internal) *)
  let api = Dm_api.filter (fun _ -> true) (fun _ -> true)
    (fun msg -> match msg.msg_tag with (FromObject (Private _)) -> false | _ -> true) api in
  let system = objects_of_api api and relations = relations_of_api api in

  let graphfilename =
    if closed then
      "xenenterpriseapi-datamodel-graph"
    else
      "xenapi-datamodel-graph" in

    print_endline "%
% Copyright (c) 2006-2007 XenSource, Inc.
%
% All rights reserved.
%
% Authors: Ewan Mellor, Richard Sharp, Dave Scott.
%

\\chapter{API Reference}
\\label{api-reference}

";

(*    print_endline "This API Reference is autogenerated from datamodel specification and IDL --- do not hand-edit.";
*)
    print_endline "\\section{Classes}";
    print_endline "The following classes are defined:";
    print_endline "";
    print_endline "\\begin{center}\\begin{tabular}{|lp{10cm}|}";
    print_endline "\\hline";
    print_endline "Name & Description \\\\";
    print_endline "\\hline";

    List.iter (fun obj -> printf "{\\tt %s} & %s \\\\\n" (escape obj.name) (escape (first_sentence obj.description))) system;

    print_endline "\\hline";
    print_endline "\\end{tabular}\\end{center}";

    print_endline "\\section{Relationships Between Classes}";
    print_endline "Fields that are bound together are shown in the following table: ";

    print_endline "\\begin{center}\\begin{tabular}{|ll|l|}";
    print_endline "\\hline";
    print_endline "{\\em object.field} & {\\em object.field} & {\\em relationship} \\\\\n";
    print_endline "\\hline";
    List.iter (function (((a, a_field), (b, b_field)) as rel) ->
		 let c = Relations.classify api rel in
		   printf "%s.%s & %s.%s & %s\\\\\n" 
		     (escape a) (escape a_field) 
		     (escape b) (escape b_field)
		     (Relations.string_of_classification c)
	      ) relations;
    print_endline "\\hline";
    print_endline "\\end{tabular}\\end{center}";
    
    print_endline "";

    print_endline "The following represents bound fields (as specified above) diagramatically, using crows-foot notation to specify one-to-one, one-to-many or many-to-many
                   relationships:";
    print_endline "";
    print_endline "\\begin{center}\\resizebox{0.8\\textwidth}{!}{"; 
    print_endline (sprintf "\\includegraphics{%s}" graphfilename);
    print_endline "}\\end{center}";
    print_endline "\\";
    print_endline "\\subsection{List of bound fields}";

    print_endline "\\section{Types}";
    print_endline "\\subsection{Primitives}";
    print_endline "The following primitive types are used to specify methods and fields in the API Reference:";
    print_endline "";
    print_endline "\\begin{center}\\begin{tabular}{|ll|}";
    print_endline "\\hline";
    print_endline "Type & Description \\\\";
    print_endline "\\hline";    
    print_endline "String & text strings \\\\";
    print_endline "Int    & 64-bit integers \\\\";
    print_endline "Float & IEEE double-precision floating-point numbers \\\\";
    print_endline "Bool   & boolean \\\\";
    print_endline "DateTime & date and timestamp \\\\";
    print_endline "Ref (object name) & reference to an object of class name \\\\";
    print_endline "\\hline";
    print_endline "\\end{tabular}\\end{center}";
    print_endline "\\subsection{Higher order types}";
    print_endline "The following type constructors are used:";
    print_endline "";
    print_endline "\\begin{center}\\begin{tabular}{|ll|}";
    print_endline "\\hline";
    print_endline "Type & Description \\\\";
    print_endline "\\hline";    
    print_endline "List (t) & an arbitrary-length list of elements of type t \\\\";
    print_endline "Map (a $\\rightarrow$ b) & a table mapping values of type a to values of type b \\\\";
    print_endline "\\hline";
    print_endline "\\end{tabular}\\end{center}";
    print_endline "\\subsection{Enumeration types}";
    print_endline "The following enumeration types are used:";
    print_endline "";
    List.iter (function Enum (name, options) -> 
		 List.iter print_endline (of_enum_alias name options);
		 print_string vgap
		 | _ -> () ) (Types.of_objects system);
    print_endline "";
    if closed then
      begin
        print_endline "\\section{Class field summary}";
        print_endline "";
        print_endline "This section summarises the fields in each class and their qualifiers. This information is replicated in the detailed class reference later in this document and is aggregated here solely for convenience.";
        print_endline "";
        List.iter (fun x -> List.iter print_endline (field_table_of_obj true x closed);
                     print_string vgap) system;
        error_section()
      end;
    List.iter (fun x -> List.iter print_endline (of_obj x closed);
                 print_string vgap) system;
    if not closed then
      begin
        error_section()
      end
