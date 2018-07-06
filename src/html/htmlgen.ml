open Rpc.Types
open Idl
open Codegen
open Cow.Html



(* Printable string of type *)
let rec html_of_t : type a.a typ -> string list =
  let of_basic : type b.b basic -> string = function
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Bool -> "bool"
    | Float -> "float"
    | String -> "string"
    | Char -> "char"
  in
  let print txt = [ txt ] in
  function
  | Basic b -> print (of_basic b)
  | DateTime -> print (of_basic String)
  | Struct _ -> print ("struct  { ... }")
  | Variant _ -> print ("variant { ... }")
  | Array t -> html_of_t t @ (print " list")
  | List t -> html_of_t t @ (print " list")
  | Dict (key, v) -> print (Printf.sprintf "(%s * " (of_basic key)) @ (html_of_t v) @ (print ") list");
  | Unit -> print "unit"
  | Option x -> html_of_t x @ (print " option")
  | Tuple (a, b) -> html_of_t a @ (print " * ") @ (html_of_t b)
  | Abstract _ -> print "<abstract>"

(* Function inputs and outputs in a table *)
let of_args args =
  let row_of_arg (is_in, Param.Boxed arg) =
    let name = arg.Param.name in
    let direction = if is_in then "in" else "out" in
    let ty = html_of_t arg.Param.typedef.ty |> List.map string in
    let description = arg.Param.description in
    tag "tr"
      (list [
          tag "td" (tag "code" (string (match name with Some s -> s | None -> "unnamed")));
          tag "td" (string direction);
          tag "td" (tag "code" (list ty));
          tag "td" (string (String.concat " " description))
      ])
  in
  tag "table" ~attrs:["width","100%"]
    (list [
        tag "thead" (tag "tr" (list [
            tag "th" (string "Name");
            tag "th" (string "Direction");
            tag "th" (string "Type");
            tag "th" (string "Description")]));
        tag "tbody" (list (List.map row_of_arg args))
      ])

let sidebar x =
  let of_typedef (BoxedDef t) =
    let target = Printf.sprintf "#a-%s" t.name in
    let name = t.name in
    tag "li" (a ~href:(Uri.of_string target) (string name))
  in
  let of_method iname (BoxedFunction m) =
    let target = Printf.sprintf "#a-%s-%s" iname m.Method.name in
    let name = m.Method.name in
    tag "li" (a ~href:(Uri.of_string target) (string name))
  in
  let of_interface i =
    let name = i.Interface.details.Idl.Interface.name in
    [tag "li" ~cls:"docs-nav-title" (string name)] @
    List.map (of_method i.Interface.details.Idl.Interface.name) i.Interface.methods
  in
  div ~cls:"large-3 medium-3 columns" (
    tag "ul" ~cls:"menu vertical" (list ([
        tag "li" ~cls:"docs-nav-title" (string "types")] @
        (List.map of_typedef x.Interfaces.type_decls) @
        (List.concat (List.map of_interface x.Interfaces.interfaces)))))

let of_struct_fields : 'a boxed_field list -> Cow.Html.t = fun all ->
  let of_row (BoxedField f) =
    let ty = html_of_t f.field in
    tag "tr"
      (list
         [ tag "td" (tag "pre" (string f.fname));
           tag "td" (tag "pre" (string (String.concat "" ty)));
           tag "td" (string (String.concat " " f.fdescription))
    ])
  in
  tag "table" ~attrs:["width","100%"]
    (list [ tag "thead"
              (tag "tr"
                 (list
                    [ tag "th" (string "Name");
                      tag "th" (string "Type");
                      tag "th" (string "Description");
                    ]));
            tag "tbody" (list (List.map of_row all)) ])

(* TODO: Unify with the above! *)
let of_variant_tags : 'a boxed_tag list -> Cow.Html.t = fun all ->
  let of_row (BoxedTag t) =
    let ty = html_of_t t.tcontents in
    tag "tr"
      (list
         [ tag "td" (tag "pre" (string t.tname));
           tag "td" (tag "pre" (string (String.concat "" ty)));
           tag "td" (string (String.concat " " t.tdescription))
    ])
  in
  tag "table" ~attrs:["width","100%"]
    (list [ tag "thead"
              (tag "tr"

                 (list
                    [ tag "th" (string "Name");
                      tag "th" (string "Type");
                      tag "th" (string "Description");
                    ]));
            tag "tbody" (list (List.map of_row all)) ])


let of_type_decl i_opt (BoxedDef t) =
  let anchor = Printf.sprintf "a-%s" t.name in
  let name = t.name in
  let defn = String.concat "" (html_of_t t.ty) in
  let description = t.description in
  let common =
    [ h4 ~id:anchor (string (Printf.sprintf "type %s = %s" name defn));
      p (string (String.concat " " description))
    ]
  in
  let rest = match t.ty with
    | Struct structure ->
      [ p (string "Members:");
        of_struct_fields structure.fields ]
    | Variant variant ->
      [ p (string "Constructors:");
        of_variant_tags variant.variants ]
    | _ -> [] in
  common @ rest

let tabs_of is i m =
  let mname = m.Method.name in
  let hash_defn = "#defn-" ^ mname in
  let hash_ocaml = "#ocaml-" ^ mname in
  let hash_python = "#python-" ^ mname in
  let id_tab = "tab-" ^ mname in
  let id_defn = "defn-" ^ mname in
  let id_ocaml = "ocaml-" ^ mname in
  let id_python = "python-" ^ mname in
  [ ul ~cls:"tabs" ~id:id_tab ~attrs:["data-tabs",""]
      [ li ~cls:"tabs-title is-active" (tag "a" ~attrs:["href",hash_defn; "aria-selected","true"] (string "Definition"));
        li ~cls:"tabs-title" (a ~href:(Uri.of_string hash_ocaml) (string "OCaml example"));
        li ~cls:"tabs-title" (a ~href:(Uri.of_string hash_python) (string "Python example"));
      ];
    div ~cls:"tabs-content" ~attrs:["data-tabs-content",id_tab]
      (list [
          div ~cls:"tabs-panel is-active" ~id:(id_defn)
            (of_args (
                 List.map (fun p -> (true,p)) Method.(find_inputs m.ty) @
                 [ (false, Method.(find_output m.ty)) ]));
          div ~cls:"tabs-panel" ~id:(id_ocaml)
            (list [
                h4 (string "Client:");
                h4 (string "Server:");
              ]);
          div ~cls:"tabs-panel" ~id:(id_python)
            (list [
                h4 (string "Client:");
                tag "pre" ~cls:"prettyprint lang-py" (string (Pythongen.example_stub_user i (BoxedFunction m) |> Pythongen.string_of_ts));
                h4 (string "Server:");
                tag "pre" ~cls:"prettyprint lang-py" (string (Pythongen.example_skeleton_user i (BoxedFunction m) |> Pythongen.string_of_ts));
              ])
        ])
  ]

let of_method is i (Codegen.BoxedFunction m) =
  let anchor = Printf.sprintf "a-%s-%s" i.Interface.details.Idl.Interface.name m.Method.name in
  let name = m.Method.name in
  let description = m.Method.description in
  [ h3 ~id:anchor (string name);
    p (string (String.concat " " description))
  ] @ tabs_of is i m


let of_interface is i =
  let name = i.Interface.details.Idl.Interface.name in
  let anchor = "a-" ^ name in
  let description = i.Interface.details.Idl.Interface.description in
  [ h2 ~id:anchor (string name);
    p (string (String.concat " " description))
  ] @ List.concat (List.map (of_method is i) i.Interface.methods)

(*
let of_exception ts =
  let row_of t =
    let ident = ident_of_type_decl t in
    let name = [ `Data (String.concat "/" ident.Ident.name) ] in
    let ty = [ `Data (Type.ocaml_of_t ident.Ident.original_ty) ] in
    let description = [ `Data ident.Ident.description ] in
    <:html<
      <tr>
        <td><pre>$name$</pre></td>
        <td><pre>$ty$</pre></td>
        <td>$description$</td>
      </tr>
    >> in
  <:html<
    <h3 id="a-exceptions">exceptions</h3>
    <table width="100%">
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Description</th>
        </tr>
     </thead>
     <tbody>
       $List.concat (List.map row_of ts)$
     </tbody>
    </table>
  >>
*)

let of_interfaces x =
  let name = x.Interfaces.name in
  let description = x.Interfaces.description in
  div ~cls:"row"
    (list
       ([
         (sidebar x);
         div ~cls:"large-9 medium-9 columns"
           (list
              ([
                h1 (string name);
                p (string (String.concat " " description));
              ] @
                List.concat (List.map (of_type_decl None) x.Interfaces.type_decls)
                @
                List.concat (List.map (of_interface x) x.Interfaces.interfaces)))]))


let to_string x = Cow.Html.to_string (of_interfaces x)
