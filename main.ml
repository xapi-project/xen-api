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

let html_navbar oc pages this_page =
  let div cls = `El_start (("", "div"), [ ("", "class"), cls ]) in
  let a cls toggle target = `El_start (("", "a"), [ ("", "class"), cls; ("", "data-toggle"), toggle; ("", "data-target"), target ]) in
  let span cls = `El_start (("", "span"), [ ("", "class"), cls ]) in
  let ul cls = `El_start (("", "ul"), [ ("", "class"), cls ]) in
  let li cls = `El_start (("", "li"), match cls with None -> [] | Some x -> [ ("", "class"), x ]) in
  let endtag = `El_end in
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
      xmlm (`El_start (("", "a"), [ ("", "class"), "brand"; ("", "href"), "#"]));
      xmlm (`Data "XCP on-host APIs");
      xmlm endtag;
      xmlm (div "nav-collapse");
      xmlm (ul "nav");
      List.iter
	(fun page ->
	  xmlm (li (if page = this_page then Some "active" else None));
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

let _ =
  let resolve_refs_in_api api =
    let idents, api = Types.lift_type_decls api in
    Types.dump_ident_mappings idents;
    idents, (Types.resolve_references idents api) in

  let apis = [
    Smapiv2.api;
    Xenops.api;
  ] in
  let pages = List.map page_of_api apis in

  List.iter
    (fun page ->
      with_file page.path
       (fun oc ->
	 print_file_to oc ("doc/header.html");
	 html_navbar oc pages page;
	 let idents, api = resolve_refs_in_api page.api in
	 output_string oc (Types.to_html idents api);
	 print_file_to oc ("doc/footer.html")
      );
    ) pages

