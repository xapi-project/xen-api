open Files
open Types
open Cow.Html

type page = {
  name: string;
  title: string;
  filename: string;
  path: string;
  description: string;
  api: Interfaces.t;
}

let uri = Uri.of_string

(* Printable string of type *)
let rec html_of_t env =
  let open Type in
  let print txt = string txt in
  function
  | Basic b -> print (ocaml_of_basic b)
  | Struct (_, _) -> print ("struct  { ... }")
  | Variant (_, _) -> print ("variant { ... }")
  | Array t -> html_of_t env t @ (print " list")
  | Dict (key, v) -> print (Printf.sprintf "(%s * " (ocaml_of_basic key)) @ (html_of_t env v) @ (print ") list");
  | Name x ->
    let ident =
      if not(List.mem_assoc x env)
      then failwith (Printf.sprintf "Unable to find ident: %s" x)
      else List.assoc x env in
    let target = uri ("#a-" ^ x) in
    let name = string (String.concat "/" ident.Ident.name) in
    a ~href:target name
  | Unit -> print "unit"
  | Option x -> html_of_t env x @ (print " option")
  | Pair(a, b) -> html_of_t env a @ (print " * ") @ (html_of_t env b)
  | Custom c -> print c

(* Function inputs and outputs in a table *)
let of_args env args =
  let row_of_arg (is_in, arg) =
    let name = string arg.Arg.name in
    let direction = string (if is_in then "in" else "out") in
    let ty = html_of_t env arg.Arg.ty in
    let description = string arg.Arg.description in
    tag "tr" (list [
        tag "td" (tag "code" name);
        tag "td" direction;
        tag "td" (tag "code" ty);
        tag "td" description;
      ])
  in
  tag "table" ~attrs:["width", "100%"] (list [
      tag "thead" (
        tag "tr" (list [
            tag "th" (string "Name");
            tag "th" (string "Direction");
            tag "th" (string "Type");
            tag "th" (string "Description");
          ])
      );
      tag "tbody" (list (List.map row_of_arg args))
    ])

let sidebar env x =
  let of_typedecl name_opt t =
    let ident = ident_of_type_decl env t in
    let target = match name_opt with
      | Some name -> Printf.sprintf "#a-%s-%s" name ident.Ident.id
      | None -> Printf.sprintf "#a-%s" ident.Ident.id
    in
    let name = string t.TyDecl.name in
    li (a ~href:(uri target) name)
  in
  let of_method name m =
    let target =
      Printf.sprintf "#a-%s-%s" name m.Method.name
      |> uri
    in
    let name = string m.Method.name in
    li (a ~href:target name)
  in
  let of_interface i =
    let name = i.Interface.name in
    list [
      list [li ~cls:"docs-nav-title" (string ("interface:" ^ name))];
      list (List.map (of_typedecl (Some i.Interface.name)) i.Interface.type_decls);
      list (List.map (of_method i.Interface.name) i.Interface.methods)
    ]
  in
  div ~cls:"large-3 medium-3 columns" (
    ul ~cls:"menu vertical" ([
        li ~cls:"docs-nav-title" (string "types");
        list (List.map (of_typedecl None) x.Interfaces.type_decls);
        list (List.map of_interface x.Interfaces.interfaces);
        li ~cls:"docs-nav-title" (string "exception");
        li (a ~href:(uri "#a-exceptions") (string "Exceptions"))
      ])
  )

let of_struct_variant_fields all =
  let of_row (name, ty, descr) =
    let name = string name in
    let ty = string (Type.ocaml_of_t ty) in
    let description = string descr in
    tag "tr" (list [
        tag "td" (tag "pre" name);
        tag "td" (tag "pre" ty);
        tag "td" description
      ])
  in
  tag "table" ~attrs:["width", "100%"] (list [
      tag "thead" (list [
          tag "th" (string "Name");
          tag "th" (string "Type");
          tag "th" (string "Description")
        ]);
      tag "tbody" (list (List.map of_row all))
    ])

let of_type_decl (env: 'a list) i_opt t =
  let ident = ident_of_type_decl env t in
  let anchor =
    match i_opt with
    | Some i -> (Printf.sprintf "a-%s-%s" i.Interface.name ident.Ident.id)
    | None -> (Printf.sprintf "a-%s" ident.Ident.id) in
  let name = t.TyDecl.name in
  let defn = Type.string_of_t ident.Ident.ty in
  let description = string t.TyDecl.description in
  let h4_text = "type " ^ name ^ " = " ^ defn in
  let common = list [
      h4 ~id:anchor (string h4_text);
      p description
    ] in
  let rest = match ident.Ident.original_ty with
    | Type.Struct(hd, tl) ->
      list [
        p (string "Members");
        of_struct_variant_fields (hd::tl)
      ]
    | Type.Variant(hd, tl) ->
      list [
        p (string "Constructors");
        of_struct_variant_fields (hd::tl)
      ]
    | _ -> [] in
  list [common; rest]

let tabs_of env is i m =
  let mname = m.Method.name in
  let hash_defn = "#defn-" ^ mname in
  let hash_ocaml = uri ("#ocaml-" ^ mname) in
  let hash_python = uri ("#python-" ^ mname) in
  let id_tab = "tab-" ^ mname in
  let id_defn = "defn-" ^ mname in
  let id_ocaml = "ocaml-" ^ mname in
  let id_python = "python-" ^ mname in
  list [
    ul ~cls:"tabs" ~attrs:["data-tabs",""] ~id:id_tab ([
        li ~cls:"tabs-title is-active" (
          (* workaround: a does not support ~attr *)
          tag "a" ~attrs:[
            "href",hash_defn;
            "aria-selected","true"
          ] (string"Definition")
        );
        li ~cls:"tabs-title" (a ~href:hash_ocaml (string "OCaml example"));
        li ~cls:"tabs-title" (a ~href:hash_python (string "Python example"))
      ]);
    div ~cls:"tabs-content" ~attrs:["data-tabs-content", id_tab] (list [
        div ~cls:"tabs-panel is-active" ~id:id_defn (
          of_args env (
            (List.map (fun m -> true, m) m.Method.inputs)
            @ (List.map (fun m -> false, m) m.Method.outputs))
        );
        div ~cls:"tabs-panel" ~id:id_ocaml (list [
            h4 (string "Client");
            string (
              Ocaml.caml2html (
                Ocaml.example_stub env is i m |> Ocaml.string_of_ts)
            );
            h4 (string "Server");
            string (
              Ocaml.caml2html (
                Ocaml.example_skeleton_user env is i m |> Ocaml.string_of_ts)
            )
          ]);
        div ~cls:"tabs-panel" ~id:id_python (list [
            h4 (string "Client");
            tag "pre" ~cls:"prettyprint lang-py" (
              string (Python.example_stub_user env i m |> Python.string_of_ts)
            );
            h4 (string "Server");
            tag "pre" ~cls:"prettyprint lang-py" (
              string (Python.example_skeleton_user env i m |> Python.string_of_ts)
            )
          ])
      ])
  ]

let of_method (env: 'a list) is i m =
  let anchor = Printf.sprintf "a-%s-%s" i.Interface.name m.Method.name in
  let name = string m.Method.name in
  let description = string m.Method.description in
  concat [
    list [h3 ~id:anchor name; p description];
    tabs_of env is i m
  ]

let of_interface (env: 'a list) is i =
  let anchor = "a-" ^ i.Interface.name in
  let name = string i.Interface.name in
  let description = string i.Interface.description in
  list [
    h2 ~id:anchor name;
    p description;
    list (List.map (of_type_decl env (Some i)) i.Interface.type_decls);
    list (List.map (of_method env is i) i.Interface.methods)
  ]

let of_exception env ts =
  let row_of t =
    let ident = ident_of_type_decl env t in
    let name = string (String.concat "/" ident.Ident.name) in
    let ty = string (Type.ocaml_of_t ident.Ident.original_ty) in
    let description = string ident.Ident.description in
    tag "tr" (list [
        tag "td" (tag "pre" name);
        tag "td" (tag "pre" ty);
        tag "td" description
      ])
  in list [
    h3 ~id:"a-exceptions" (string "exceptions");
    tag "table" ~attrs:["width", "100%"] (list [
        tag "thead" (list [
            tag "tr" (list [
                tag "th" (string "Name");
                tag "th" (string "Type");
                tag "th" (string "Description")
              ])
          ]);
        tag "tbody" (list (List.map row_of ts))
      ])
  ]

let of_interfaces env x =
  let name = string x.Interfaces.name in
  let description = string x.Interfaces.description in
  div ~cls:"row" (list [
      sidebar env x;
      list[
        div ~cls:"large-9 medium-9 columns" (list [
            list [
              h1 name; p description
            ];
            list (List.map (of_type_decl env None) x.Interfaces.type_decls);
            list (List.map (of_interface env x) x.Interfaces.interfaces);
            of_exception env x.Interfaces.exn_decls;
          ])
      ]
    ])

let to_string env x = to_string (of_interfaces env x)

let topbar pages =
  let link_of_page page =
    let html = uri (page.name ^ ".html") in
    let name = string page.name in
    li (a ~href:html name)
  in
  list [
    div ~cls:"title-bar" ~attrs:[
      "data-responsive-toggle","main-menu";
      "data-hide-for","medium"]
      (list[
          tag "button" ~cls:"menu-icon" ~attrs:[
            "type","button";
            "data-toggle",""] empty;
          div ~cls:"title-bar-title" (string "Menu")
        ]);
    div ~cls:"top-bar" ~id:"main-menu" (list[
        div ~cls:"top-bar-left" (
          ul ~cls:"menu" ~attrs:["data-dropdown-menu",""] [
            li ~cls:"menu-text" (string "SMAPIv3")
          ]
        );
        div ~cls:"top-bar-right" (
          ul ~cls:"menu" ~attrs:["data-responsive-menu","drilldown medium-dropdown"] ([
              li ~cls:"has-submenu" (list [
                  a ~href:(uri "features.html") (string "Learn");
                  ul ~cls:"submenu menu vertical" ~attrs:["data-submenu",""] ([
                      li (a ~href:(uri "features.html") (string "Features"));
                      li (a ~href:(uri "concepts.html") (string "Concepts"));
                      li (a ~href:(uri "architecture.html") (string "Architecture"));
                      li (a ~href:(uri "faq.html") (string "FAQ"))
                    ])
                ]);
              li (list [
                  a ~href:(uri "#") (string "Develop");
                  ul ~cls:"submenu menu vertical" (List.map link_of_page pages)
                ]);
              li (list [
                  a ~href:(uri "#") (string "Support");
                  ul ~cls:"submenu menu vertical" [
                    li (a ~href:(uri "contact.html") (string "Mailing list"));
                    li (a ~href:(uri "contact.html") (string "Issue tracker"));
                    li (a ~href:(uri "contact.html") (string "IRC"))
                  ]
                ]);
              li ~cls:"active" (a ~href:(uri "getstarted.html") (string "Get Started"))
            ])
        )
      ])
  ]

let index_html oc pages =
  let header = header (list [
      div ~cls:"row" (
        div ~cls:"large-12 columns" (list [
            h1 (string "Xapi storage interface");
            h3 ~cls:"subheader" (
              string "An easy way to connect "
              ++ a ~href:(uri "http://www.xenproject.org/developers/teams/xapi.html")
                (string "Xapi")
              ++ string " to any storage type."
            );
            hr empty;
            h2 (string "Who is this for?");
            p (string
                 "This is for anyone who has a storage system which is not \
                  supported by xapi out-of-the-box.")
          ]));
      div ~cls:"row" (list [
          div ~cls:"large-6 columns" (
            img ~alt:"Your bit here" (uri "img/your-bit-here.svg")
          );
          div ~cls:"large-6 columns" (list [
              p (string
                   "This is also for anyone who wants to manage their \
                    storage in a customized way. If you can make your volumes appear \
                    as Linux block devices "
                 ++ i (string "or")
                 ++ string " you can refer to the volumes via URIs of the form "
                 ++ tt (string "iscsi://")
                 ++ string " "
                 ++ tt (string "nfs://")
                 ++ string " or "
                 ++ tt (string "rbd://")
                 ++ string " then this documentation is for you."
                );
              p ~attrs:["style","font-weight: bold;"]
                (string "No Xapi or Xen specific knowledge is required.")
            ])
        ]);
      div ~cls:"row" (
        div ~cls:"large-12 columns panel callout" (list [
            h2 (string "Status of this documentation");
            p (string
                 "This documentation is a draft intended for discussion only. \
                  Please: ");
            ul ([
                li (
                  string "view the "
                  ++ a ~href:(uri "https://github.com/xapi-project/xapi-storage/issues")
                    (string "issues on github")
                  ++ string " or"
                );
                li (list [
                    string "join the ";
                    a ~href:(uri "http://lists.xenproject.org/mailman/listinfo/xen-api")
                      (string "mailing list")
                  ])
              ])
          ])
      )
    ])
  in
  print_file_to oc ("../static/header.html");
  output_string oc (Cow.Html.to_string (topbar pages));
  output_string oc (Cow.Html.to_string header);
  print_file_to oc ("../static/footer.html")

let placeholder_html oc pages body =
  let header = div ~cls:"row" (
      div ~cls:"large-12 columns panel callout"
        (p (string "This is a placeholder"))
    ) in
  print_file_to oc ("../static/header.html");
  output_string oc (Cow.Html.to_string (topbar pages));
  if Sys.file_exists body
  then print_file_to oc body
  else output_string oc (Cow.Html.to_string header);
  print_file_to oc ("../static/footer.html")

let page_of_api api = {
  name = api.Interfaces.name;
  title = api.Interfaces.title;
  path = api.Interfaces.name ^ ".html";
  filename = api.Interfaces.name ^ ".html";
  description = api.Interfaces.description;
  api = api;
}

let write apis =
  let pages = List.map page_of_api apis in

  List.iter
    (fun page ->
       with_output_file page.path
         (fun oc ->
            print_file_to oc ("../static/header.html");
            output_string oc (Cow.Html.to_string (topbar pages));
            let idents, api = Types.resolve_refs_in_api page.api in
            output_string oc (to_string idents api);
            print_file_to oc ("../static/footer.html")
         );
    ) pages;
  with_output_file "../index.html"
    (fun oc ->
       index_html oc pages
    );
  List.iter
    (fun placeholder ->
       let out_filename = Printf.sprintf "%s" placeholder in
       let in_filename = Printf.sprintf "../templates/%s.body" placeholder in
       with_output_file out_filename
         (fun oc ->
            placeholder_html oc pages in_filename
         )
    ) [
    "contact.html";
    "concepts.html";
    "getstarted.html";
    "features.html";
    "faq.html";
    "learn.html";
    "architecture.html";
  ]

let _ =
  let open Types in
  let open Files in
  let apis = [ Plugin.api; Control.api; Data.api; ] in
  (* Prepend the debug_info argument *)
  let apis = List.map Types.prepend_dbg apis in
  write apis
