open Lwt
open Js

let post_xml url contents =

  let method_ = "POST" in
  let content_type = "text/xml" in
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in

  req##_open (Js.string method_, Js.string url, Js._true);
  req##setRequestHeader (Js.string "Content-type", Js.string content_type);

(*  List.iter (fun (n, v) -> req##setRequestHeader (Js.string n, Js.string v)) headers;*)

  req##onreadystatechange <- Js.wrap_callback
    (fun _ ->
       (match req##readyState with
		   | XmlHttpRequest.DONE ->
			   Lwt.wakeup w
				   {XmlHttpRequest.url = url;
					code = req##status;
					content = Js.to_string req##responseText;
					content_xml =
						   (fun () ->
							   match Js.Opt.to_option (req##responseXML) with
								   | None -> None
								   | Some doc ->
									   if (Js.some doc##documentElement) == Js.null
									   then None
									   else Some doc);
					headers = fun _ -> None;
				   }
		   | _ -> ()));

  req##send (Js.some (Js.string contents));

  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res
