open Files
open Types

type page = {
  name: string;
  title: string;
  filename: string;
  path: string;
  description: string;
  api: Interfaces.t;
}

let to_string env x =
  let open Xmlm in
  let buffer = Buffer.create 128 in
  let output = Xmlm.make_output ~nl:true ~indent:(Some 4) (`Buffer buffer) in
  Xmlm.output output (`Dtd None);
  Buffer.reset buffer;
  let wrap ?id name body =
    let attrs = match id with
      | None -> []
      | Some id -> [ ("", "id"), id ] in
    Xmlm.output output (`El_start (("", name), attrs));
    Xmlm.output output (`Data body);
    Xmlm.output output (`El_end) in
  let h1 ?id = wrap ?id "h1" in
  let h2 ?id = wrap ?id "h2" in
  let h3 ?id = wrap ?id "h3" in
  let td = wrap "td" in
  let pre ?lang txt =
    let cls = match lang with
      | None -> "prettyprint"
      | Some x -> Printf.sprintf "prettyprint lang-%s" x in
    Xmlm.output output (`El_start (("", "pre"), [ ("", "class"), cls ]));
    Xmlm.output output (`Data txt);
    Xmlm.output output (`El_end) in
  let code ?id = wrap ?id "code" in
  let wrapf ?cls name items =
    let attrs = match cls with
      | None -> []
      | Some cls -> [ ("", "class"), cls ] in
    Xmlm.output output (`El_start (("", name), attrs));
    items ();
    Xmlm.output output (`El_end) in
  let tdcode x = wrapf "td" (fun () -> code x) in
  let th = wrapf "th" in
  let tr = wrapf "tr" in
  let ul ?cls = wrapf ?cls "ul" in
  let li ?cls = wrapf ?cls "li" in
  let label name = wrapf "li" (fun () -> wrap "label" name) in
  let p = wrap "p" in
  let a_href ?toggle ?icon link txt =
    let toggle = match toggle with
      | None -> []
      | Some x -> [ ("", "data-toggle"), x ] in
    Xmlm.output output (`El_start (("", "a"), [ ("", "href"), link ] @ toggle));
    begin match icon with | None -> () | Some icon ->
      Xmlm.output output (`El_start (("", "i"), [ ("", "class"), icon ]));
      Xmlm.output output (`Data "");
      Xmlm.output output (`El_end)
    end;

    Xmlm.output output (`Data txt);
    Xmlm.output output (`El_end) in

  let rec html_of_t =
    let open Type in
    let print txt = Xmlm.output output (`Data txt) in
    function
    | Basic b -> print (ocaml_of_basic b)
    | Struct (_, _) -> print ("struct  { ... }")
    | Variant (_, _) -> print ("variant { ... }")
    | Array t -> html_of_t t; print " list"
    | Dict (key, v) -> print (Printf.sprintf "(%s * " (ocaml_of_basic key)); html_of_t v; print ") list";
    | Name x ->
      let ident =
        if not(List.mem_assoc x env)
        then failwith (Printf.sprintf "Unable to find ident: %s" x)
        else List.assoc x env in
      a_href (Printf.sprintf "#a-%s" x) (String.concat "/" ident.Ident.name)
    | Unit -> print "unit"
    | Option x -> html_of_t x; print " option"
    | Pair(a, b) -> html_of_t a; print " * "; html_of_t b
    | Custom c -> print c in

  let of_args args =
    let row_of_arg (is_in, arg) =
      let name = [ `Data arg.Arg.name ] in
      let direction = [ `Data (if is_in then "in" else "out") ] in
      let ty = [ `Data "umm" ] in
      let description = [ `Data arg.Arg.description ] in
    <:html<
      <tr>
        <td><code>$name$</code></td>
        <td>$direction$</td>
        <td><code>$ty$</code></td>
        <td>$description$</td>
      </tr>
    >> in

  <:html<
  <table width="100%">
  <thead>
    <tr>
      <th>Name</th>
      <th>Direction</th>
      <th>Type</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    $List.concat (List.map row_of_arg args)$
  </tbody>
  </table>
  >> in

  (* Main content *)

  Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "row" ]));
  (* Side bar *)
  Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "large-3 medium-3 columns" ]));
  ul ~cls:"side-nav" (fun () ->
      label "types";
      List.iter (fun t ->
          let ident = ident_of_type_decl env t in
          li (fun () ->
              a_href (* ~toggle:"tab" *) (Printf.sprintf "#a-%s" ident.Ident.id) t.TyDecl.name
            )
        ) x.Interfaces.type_decls;
      List.iter (fun i ->
          label i.Interface.name;
          List.iter (fun t ->
              let ident = ident_of_type_decl env t in
              li (fun () ->
                  a_href (* ~toggle:"tab" *) (Printf.sprintf "#a-%s" ident.Ident.id) t.TyDecl.name
                )
            ) i.Interface.type_decls;
          List.iter (fun m ->
              li (fun () ->
                  a_href (* ~toggle:"tab" *) (Printf.sprintf "#a-%s" m.Method.name) m.Method.name
                )
            ) i.Interface.methods
        ) x.Interfaces.interfaces;
      label "exceptions";
      li ~cls:"nav-header" (fun () ->
          a_href "#a-exceptions" "Exceptions";
        );
(*
	List.iter (fun t ->
	  li (fun () ->
	    let ident = ident_of_type_decl env t in
	    a_href (Printf.sprintf "#a-%s" ident.Ident.id) t.TyDecl.name
	  )
	) x.Interfaces.exn_decls;
*)
    );
  Xmlm.output output (`El_end);

  let of_struct_variant_fields all =
    Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped table-condensed" ]));
    th (fun () -> td "Type"; td "Description");
    List.iter
      (fun (name, ty, descr) ->
         tr (fun () ->
             tdcode name;
             tdcode (Type.ocaml_of_t ty);
             td descr
           )
      ) all;
    Xmlm.output output (`El_end) in
  let of_type_decl t =
    let ident = ident_of_type_decl env t in
    h2 ~id:(Printf.sprintf "a-%s" ident.Ident.id) (Printf.sprintf "type %s = %s" t.TyDecl.name (Type.string_of_t ident.Ident.ty));
    p t.TyDecl.description;
    match ident.Ident.original_ty with
    | Type.Struct(hd, tl) ->
      p "Members:";
      of_struct_variant_fields (hd :: tl)
    | Type.Variant(hd, tl) ->
      p "Constructors:";
      of_struct_variant_fields (hd :: tl)
    | _ -> () in

  Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "large-9 medium-9 columns" ]));

  List.iter of_type_decl x.Interfaces.type_decls;
  List.iter
    (fun i ->
       h2 ~id:(Printf.sprintf "a-%s" i.Interface.name) i.Interface.name;
       p i.Interface.description;
       List.iter of_type_decl i.Interface.type_decls;
       List.iter
         (fun m ->
            h3 ~id:(Printf.sprintf "a-%s" m.Method.name) m.Method.name;
            p m.Method.description;
            let mname = [ `Data m.Method.name ] in
            let hash_defn = [ `Data ("#defn-" ^ m.Method.name) ] in
            let hash_ocaml = [ `Data ("#ocaml-" ^ m.Method.name) ] in
            let hash_python = [ `Data ("#python-" ^ m.Method.name) ] in
            let id_defn = [ `Data ("defn-" ^ m.Method.name) ] in
            let id_ocaml = [ `Data ("ocaml-" ^ m.Method.name) ] in
            let id_python = [ `Data ("python-" ^ m.Method.name) ] in
            let tabs = <:html<
              <dl class="tabs" data-tab="">
                <dd class="active"><a href="$hash_defn$">Definition</a></dd>
                <dd><a href="$hash_ocaml$">OCaml example</a></dd>
                <dd><a href="$hash_python$">Python example</a></dd>
              </dl>
              <div class="tabs-content">
                <div class="content active" id="$id_defn$">
                  $ of_args ((List.map (fun m -> true, m) m.Method.inputs) @
                             (List.map (fun m -> false, m) m.Method.outputs)) $
                </div>
                <div class="content" id="$id_ocaml$">
                  <h4>Client</h4>
                  $ Cow.Html.of_string (Ocaml.caml2html (Ocaml.example_stub env x i m |> Ocaml.string_of_ts)) $
                  <h4>Server</h4>
                  $ Cow.Html.of_string (Ocaml.caml2html (Ocaml.example_skeleton_user env x i m |> Ocaml.string_of_ts)) $
                </div>
                <div class="content" id="$id_python$">
                  <h4>Client</h4>
                  <pre class="prettyprint lang-py">
                  $[ `Data (Python.example_stub_user env i m |> Python.string_of_ts) ]$
                  </pre>
                  <h4>Server</h4>
                  <pre class="prettyprint lang-py">
                  $[ `Data (Python.example_skeleton_user env i m |> Python.string_of_ts) ]$
                  </pre>
                </div>
              </div>
            >> in
            (* of_args (List.map (fun m -> true, m) m.Method.inputs @ (List.map (fun m -> false, m) m.Method.outputs)); *)
            Buffer.add_string buffer (Cow.Html.to_string tabs)
         ) i.Interface.methods;
    ) x.Interfaces.interfaces;

  h1 ~id:"a-exceptions" "Exceptions";

  Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped table-condensed" ]));
  th (fun () -> td "Parameter Types"; td "Description");
  List.iter
    (fun t ->
       let ident = ident_of_type_decl env t in
       tr (fun () ->
           tdcode (String.concat "/" ident.Ident.name);
           tdcode (Type.ocaml_of_t ident.Ident.original_ty);
           td ident.Ident.description
         )
    ) x.Interfaces.exn_decls;
  Xmlm.output output (`El_end);

  Xmlm.output output (`El_end);
  Xmlm.output output (`El_end);
  Buffer.contents buffer


module Html = struct

  let div cls = `El_start (("", "div"), [ ("", "class"), cls ])
  let a cls toggle target = `El_start (("", "a"), [ ("", "class"), cls; ("", "data-toggle"), toggle; ("", "data-target"), target ])
  let span cls = `El_start (("", "span"), [ ("", "class"), cls ])
  let ul cls = `El_start (("", "ul"), [ ("", "class"), cls ])
  let li cls = `El_start (("", "li"), match cls with None -> [] | Some x -> [ ("", "class"), x ])
  let h1 = `El_start (("", "h1"), [])
  let h2 = `El_start (("", "h2"), [])
  let p = `El_start (("", "p"), [])
  let endtag = `El_end
end

let topbar pages =
  let link_of_page page =
    let html = [ `Data (page.name ^ ".html") ] in
    let name = [ `Data page.name ] in
    <:html<
      <li><a href="$html$">$name$</a></li>
    >> in
<:html<
<nav class="top-bar" data-topbar="" role="navigation">
  <ul class="title-area">
    <li class="name">
      <h1><a href="index.html">Xapi storage interface</a></h1>
    </li>
    <li class="toggle-topbar menu-icon"><a href="#"><span>Menu</span></a></li>
  </ul>
  <section class="top-bar-section">
    <ul class="right">
      <li class="has-dropdown">
        <a href="#">Learn</a>
        <ul class="dropdown">
          <li><a href="#">Features</a></li>
          <li><a href="#">FAQ</a></li>
        </ul>
      </li>
      <li class="has-dropdown">
        <a href="#">Develop</a>
        <ul class="dropdown">
          <li><a href="#">Concepts</a></li>
          $List.concat (List.map link_of_page pages)$
        </ul>
      </li>
      <li class="has-dropdown">
        <a href="#">Support</a>
        <ul class="dropdown">
          <li><a href="#">Mailing list</a></li>
          <li><a href="#">Issue tracker</a></li>
          <li><a href="#">IRC</a></li>
        </ul>
      </li>
      <li class="active"><a href="#">Get Started</a></li>
    </ul>
  </section>
</nav>
>>


let index_html oc pages =
  let open Html in
  let header = <:html<
  <header>
    <div class="row">
      <div class="large-12 columns">
        <h1>Xapi storage interface</h1>
        <p>An easy way to connect <a href="http://www.xenproject.org/developers/teams/xapi.html">Xapi</a> to any storage type.</p>
        <h4>Who is this for?</h4>
        <p>This is for anyone who has a storage system which is not supported
           by xapi out-of-the-box, or for anyone who wants to manage their
           storage in a customized way. If you can make your volumes appear
           as Linux block devices <i>or</i> you can refer to the volumes via
           URIs of the form <tt>iscsi://</tt> <tt>nfs://</tt> or <tt>rbd://</tt>then
           this documentation is for you.</p>
        <p>No Xapi or Xen specific knowledge
           is required.</p>
      </div>
    </div>
    <div class="row">
      <div class="large-12 columns panel callout">
        <h2>Status of this documentation</h2>
        <p>This documentation is a draft intended for discussion only.
           Please:</p>
        <ul>
          <li>view the <a href="https://github.com/djs55/xapi-storage/issues">issues on github</a></li> or
          <li>join the <a href="http://lists.xenproject.org/mailman/listinfo/xen-api">mailing list</a></li>
        </ul>
      </div>
    </div>
  </header>
  >> in
  print_file_to oc ("doc/header.html");
  output_string oc (Cow.Html.to_string (topbar pages));
  output_string oc (Cow.Html.to_string header);
  print_file_to oc ("doc/footer.html")

let page_of_api api = {
  name = api.Interfaces.name;
  title = api.Interfaces.title;
  path = "doc/" ^ api.Interfaces.name ^ ".html";
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
            print_file_to oc ("doc/header.html");
            output_string oc (Cow.Html.to_string (topbar pages));
            let idents, api = Types.resolve_refs_in_api page.api in
            output_string oc (to_string idents api);
            print_file_to oc ("doc/footer.html")
         );
    ) pages;
  with_output_file "doc/index.html"
    (fun oc ->
       index_html oc pages
    )
