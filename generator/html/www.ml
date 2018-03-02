open Files
open Cow.Html

type page = {
  name: string;
  title: string;
  filename: string;
  path: string;
  description: string;
  api: Codegen.Interfaces.t;
}

let uri = Uri.of_string

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
  print_file_to oc ("../../../../doc/static/header.html");
  output_string oc (Cow.Html.to_string (topbar pages));
  output_string oc (Cow.Html.to_string header);
  print_file_to oc ("../../../../doc/static/footer.html")

let placeholder_html oc pages body =
  let header = div ~cls:"row" (
      div ~cls:"large-12 columns panel callout"
        (p (string "This is a placeholder"))
    ) in
  print_file_to oc ("../../../../doc/static/header.html");
  output_string oc (Cow.Html.to_string (topbar pages));
  if Sys.file_exists body
  then print_file_to oc body
  else output_string oc (Cow.Html.to_string header);
  print_file_to oc ("../../../../doc/static/footer.html")

let page_of_api api = {
  name = api.Codegen.Interfaces.name;
  title = api.Codegen.Interfaces.title;
  path = "../../../../doc/gen/" ^ api.Codegen.Interfaces.name ^ ".md";
  filename = api.Codegen.Interfaces.name ^ ".md";
  description = String.concat "" api.Codegen.Interfaces.description;
  api = api;
}

let write apis =
  let pages = List.map page_of_api apis in

  List.iter
    (fun page ->
       with_output_file page.path
         (fun oc ->
            output_string oc (Markdowngen.to_string page.api);
         );
    ) pages;
  with_output_file "../../../../index.html"
    (fun oc ->
       index_html oc pages
    );
  List.iter
    (fun placeholder ->
       let out_filename = Printf.sprintf "%s" placeholder in
       let in_filename = Printf.sprintf "../../../../templates/%s.body" placeholder in
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
  (* Prepend the debug_info argument *)
  write Apis.apis
