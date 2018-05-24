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
(** Workload Balancing Reports and Diagnostics.
 * @group Workload Balancing
*)

(**
   This module serves the /wlb_report and /wlb_diagnostics HTTP requests.
   In the former case, we receive some basic parameters (report name, report
   params) and pass those to the WLB server as a SOAP request.  The latter
   takes no parameters, but is also a SOAP request.

   What comes back is a SOAP response, containing the report data as the
   result.   The result itself is an XML string, so this ends up as XML
   escaped inside an envelope of SOAP/XML cruft.

   The response could potentially be large (megabytes), so we have to stream to
   avoid OCaml's 16MB string limit.  We can't use Xmlm, even in streaming mode,
   because it's just one very large node, so we hit the same limit.

   What we do instead is have a receive-side state machine, which passes
   through three states:
    +  Looking for the <XmlDataSet> tag, and discarding data.
    +  Looking for the </XmlDataSet> tag, and sending data.
    +  Discarding data until EOF.

   When sending, we have a separate two-state machine for entity decode:
    +  Looking for an ampersand, and sending data.
    +  Found an ampersand, so looking for the ending semicolon.

   If the response does not contain an <XmlDataSet> node, then it's most
   likely a WLB error response.  We parse these using the normal XML parser,
   through the routines in Workload_balancing.  (Error responses are never
   large.)

   If it parses through neither method, then it's malformed, and we raise an
   appropriate exception.

   The GetDiagnostics message is identical, except we look for different
   start and end tags.
*)

(*
  <!-- ExecuteReport response-->
  <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
    <s:Header>
      <a:Actions:mustUnderstand="1">http://schemas.citrix.com/DWM/IWorkloadBalance/ExecuteReportResponse</a:Action>
      <a:RelatesTo>urn:uuid:5e6fdf1b-db8d-4b67-82e0-3767646ff328</a:RelatesTo>
    </s:Header>
    <s:Body>
      <ExecuteReportResponse xmlns="http://schemas.citrix.com/DWM">
        <ExecuteReportResult xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
          <XmlDataSet>
            &lt;NewDataSet&gt;&#xD;.  &lt;xs:schema id="NewDataSet" xmlns=""
...
...
...
            &lt;/Table1&gt;&#xD;.&lt;/NewDataSet&gt;
          </XmlDataSet>
        </ExecuteReportResult>
      </ExecuteReportResponse>
    </s:Body>
  </s:Envelope>

  <!-- Invalid ExecuteReport response-->
  <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
    <s:Header>
      <a:Action s:mustUnderstand="1">http://schemas.citrix.com/DWM/IWorkloadBalance/ExecuteReportResponse</a:Action>
      <a:RelatesTo>urn:uuid:9e7642a8-3267-4be5-a594-a26a90bcadbf</a:RelatesTo>
    </s:Header>
    <s:Body>
      <ExecuteReportResponse xmlns="http://schemas.citrix.com/DWM">
        <ExecuteReportResult xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
          <ResultCode>4007</ResultCode>
        </ExecuteReportResult>
      </ExecuteReportResponse>
    </s:Body>
  </s:Envelope>

 *)

open Printf

open Http
open Stdext.Xstringext

module D = Debug.Make(struct let name="wlb_reports" end)
open D

let report_tag = "XmlDataSet"
let diagnostics_tag = "DiagnosticData"

let bufsize = 16384

let trim_and_send method_name tag recv_sock send_sock =
  let recv_buf = Buffer.create bufsize in
  let fill () =
    let s = Bytes.create bufsize in
    let n = Unix.read recv_sock s 0 bufsize in
    if n > 0 then
      Buffer.add_bytes recv_buf (Bytes.sub s 0 n);
    n
  in
  (* Since we use xml parser to parse the reponse message, we don't need to escape the xml content in `send` *)
  let send s =
    Unix.write_substring send_sock s 0 (String.length s) |> ignore
  in
  let rec recv_all ()=
    let n = fill() in
    if n > 0 then
      recv_all()
    else
      ()
  in
  recv_all();
  let s = Buffer.contents recv_buf in
  debug "receive len: %d, content: %s" (String.length s) s;
  try
    let xml_data = Xml.parse_string s in
    let report_result_xml = Workload_balancing.retrieve_inner_xml method_name xml_data true in
    try
      let xml_data_set_content = Workload_balancing.data_from_leaf (Workload_balancing.descend_and_match [tag] report_result_xml) in
      debug "send conent: %s" xml_data_set_content;
      send xml_data_set_content
    with
    | Workload_balancing.Xml_parse_failure error ->
      Workload_balancing.parse_result_code
        method_name
        report_result_xml
        "Failed to detect end of XML, data could be truncated"
        s
        true
  with
  | Xml.Error err ->
    Workload_balancing.raise_malformed_response' method_name "Expected data is truncated." s


let handle req bio method_name tag (method_name, request_func) =
  let client_sock = Buf_io.fd_of bio in
  Buf_io.assert_buffer_empty bio;
  debug "handle: fd = %d" (Stdext.Unixext.int_of_file_descr client_sock);
  req.Request.close <- true;

  Xapi_http.with_context (sprintf "WLB %s request" method_name) req
    client_sock
    (fun __context ->

       (* This is the signal to say we've taken responsibility from the CLI server for completing the task *)
       (* The GUI can deal with this itself, but the CLI is complicated by the thin cli/cli server split *)
       TaskHelper.set_progress ~__context 0.0;

       let parse response wlb_sock =
         Http_svr.headers client_sock (Http.http_200_ok ());
         trim_and_send method_name tag wlb_sock client_sock
       in
       try
         request_func ~__context ~handler:parse
       with
       | Api_errors.Server_error (_, _) as exn ->
         raise exn
       | exn ->
         warn "WLB %s request failed: %s" method_name
           (ExnHelper.string_of_exn exn);
         raise (Api_errors.Server_error (Api_errors.internal_error, []))
    )


(* GET /wlb_report?session_id=<session>&task_id=<task>&
                   report=<report name>&<param1>=<value1>&...
*)
let report_handler (req: Request.t) (bio: Buf_io.t) _ =
  if not (List.mem_assoc "report" req.Request.query) then
    begin
      error "Request for WLB report lacked 'report' parameter";
      failwith "Bad request"
    end;

  let report = List.assoc "report" req.Request.query in
  let params =
    List.filter
      (fun (k, _) ->
         not (List.mem k ["session_id"; "task_id"; "report"]))
      req.Request.query
  in
  handle req bio "ExecuteReport" report_tag
    (Workload_balancing.wlb_report_request report params)


(* GET /wlb_diagnostics?session_id=<session>&task_id=<task> *)
let diagnostics_handler (req: Request.t) (bio: Buf_io.t) _ =
  handle req bio "GetDiagnostics" diagnostics_tag
    Workload_balancing.wlb_diagnostics_request
