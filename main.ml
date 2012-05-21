open Pervasiveext
open Types

let with_file filename f =
  let oc = open_out filename in
  finally (fun () -> f oc) (fun () -> close_out oc)

let print_file_to oc =
  let output_line oc txt = output_string oc txt; output_string oc "\n" in
  Unixext.file_lines_iter (output_line oc)

type page = {
  name: string;
  title: string;
  filename: string;
  path: string;
  description: string;
  api: Interfaces.t;
}

let page_of_api api =
  let open Types in {
    name = api.Interfaces.name;
  title = api.Interfaces.title;
  path = "doc/" ^ api.Interfaces.name ^ ".html";
  filename = api.Interfaces.name ^ ".html";
  description = api.Interfaces.description;
  api = api;
}

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

let html_navbar oc pages this_page =
  let open Html in
  let txt = Types.with_xmlm
    (fun xmlm ->
      xmlm (div "navbar navbar-fixed-top");
      xmlm (div "navbar-inner");
      xmlm (div "container");
      xmlm (a "btn btn-navbar" "collapse" ".nav-collapse");
      xmlm (span "icon-bar"); xmlm endtag;
      xmlm (span "icon-bar"); xmlm endtag;
      xmlm (span "icon-bar"); xmlm endtag;
      xmlm endtag;
      xmlm (`El_start (("", "a"), [ ("", "class"), "brand"; ("", "href"), "index.html"]));
      xmlm (`Data "XCP host services");
      xmlm endtag;
      xmlm (div "nav-collapse");
      xmlm (ul "nav");
      List.iter
	(fun page ->
	  xmlm (li (if Some page = this_page then Some "active" else None));
	  xmlm (`El_start (("", "a"), [ ("", "href"), page.filename ]));
	  xmlm (`Data page.name);
	  xmlm endtag;
	  xmlm endtag
	) pages;
      xmlm endtag;
      xmlm endtag;
      xmlm endtag;
      xmlm endtag;
      xmlm endtag
    ) in
  output_string oc txt

let index_html oc pages =
  let open Html in
  let txt = Types.with_xmlm
    (fun xmlm ->
      xmlm (div "container");
      xmlm (div "hero-unit");
      xmlm h1;
      xmlm (`Data "XCP host services");
      xmlm endtag;
      xmlm p;
      xmlm (`Data "XCP is built from a number of separate services including: a domain manager, a storage manager, a host networking manager and a statistics collector. In the future some of these services will be located within isolated domains, which means they can only communicate via explicit APIs. This site contains the prototype API definitions and example code.");
      xmlm endtag; (* p *)
      xmlm (`El_start (("", "a"), [("", "class"), "btn btn-primary btn-large"; ("", "href"), "https://github.com/xen-org"]));
      xmlm (`El_start (("", "i"), [("", "class"), "icon-github-sign icon-large"]));
      xmlm (`Data "");
      xmlm endtag; (* i *)
      xmlm (`Data "View Project on GitHub");
      xmlm endtag; (* a *)
      xmlm endtag; (* hero-unit *)
      (* Make rows of 3 elements each *)
      let rec make_rows = function
	| a :: b :: c :: rest -> [a; b; c] :: (make_rows rest)
	| a :: b :: [] -> [[a; b]]
	| a :: [] -> [[a]]
	| [] -> [] in
      List.iter
	(fun row ->
	  xmlm (div "row");
	  List.iter
	    (fun page ->
	      xmlm (div "span4");
	      xmlm h2;
	      xmlm (`Data page.title);
	      xmlm endtag;
	      xmlm p;
	      xmlm (`Data page.description);
	      xmlm endtag;
	      xmlm p;
	      xmlm (`El_start (("", "a"), [("", "class"), "btn btn-info"; ("", "href"), page.filename]));
	      xmlm (`El_start (("", "i"), [("", "class"), "icon-info-sign"]));
	      xmlm (`Data "View details »");
	      xmlm endtag;
	      xmlm endtag;
	      xmlm endtag;
	      xmlm endtag;
	    ) row;
	  xmlm endtag;
	) (make_rows pages);
      xmlm endtag (* container *)
    ) in
  print_file_to oc ("doc/header.html");
  html_navbar oc pages None;
  output_string oc txt;
  print_file_to oc ("doc/footer.html")

let _ =
  let resolve_refs_in_api api =
    let idents, api = Types.lift_type_decls api in
    Types.dump_ident_mappings idents;
    idents, (Types.resolve_references idents api) in

  let apis = [
    Smapiv2.api;
    Xenops.api;
    Memory.api;
  ] in
  let pages = List.map page_of_api apis in

  List.iter
    (fun page ->
      with_file page.path
       (fun oc ->
	 print_file_to oc ("doc/header.html");
	 html_navbar oc pages (Some page);
	 let idents, api = resolve_refs_in_api page.api in
	 output_string oc (Types.to_html idents api);
	 print_file_to oc ("doc/footer.html")
      );
    ) pages;
  with_file "doc/index.html"
    (fun oc ->
      index_html oc pages
    );
  List.iter
    (fun api ->
     with_file (Printf.sprintf "python/%s.py" api.Interfaces.name)
       (fun oc ->
	 let idents, api = resolve_refs_in_api api in
	 output_string oc (Types.To_python.of_interfaces idents api |> Types.To_python.string_of_ts)
       )
    ) apis

