(*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(* Generator of C bindings from the datamodel *)


open Xapi_stdext_std.Xstringext
open Xapi_stdext_unix
open Xapi_stdext_pervasives.Pervasiveext
open Printf
open Datamodel_types
open Dm_api
open CommonFunctions

module TypeSet = Set.Make(struct
    type t = Datamodel_types.ty
    let compare = compare
  end)


let open_source'   = ref false
let destdir'       = ref ""
let templates_dir' = ref ""

let _ =
  Arg.parse
    [
      "-t", Arg.Set_string templates_dir', "specifies the firectory with the mustache templates to use";
      "-o", Arg.Set open_source', "requests a version of the API filtered for open source";
      "-d", Arg.Set_string destdir', "specifies the destination directory for the generated files";
    ]
    (fun x -> raise (Arg.Bad ("Found anonymous argument " ^ x)))
    ("Generates C bindings for the XenAPI. See -help.")

let open_source = !open_source'
let destdir = !destdir'
let templates_dir = !templates_dir'


let api =
  Datamodel_utils.named_self := true;

  let obj_filter _ = true in
  let field_filter field =
    (not field.internal_only) &&
    (if open_source then List.mem "3.0.3" field.release.opensource
     else List.mem "closed" field.release.internal)
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg &&
    (not msg.msg_hide_from_docs) &&
    (if open_source then
       (List.mem "3.0.3" msg.msg_release.opensource)
     else
       (List.mem "closed" msg.msg_release.internal)
       && (msg.msg_name <> "get")
       && (msg.msg_name <> "get_data_sources"))
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api))

let classes = objects_of_api api


module StringSet = Set.Make(struct
    type t = string
    let compare = String.compare
  end)

let enums = ref TypeSet.empty
let maps = ref TypeSet.empty
let enum_maps = ref TypeSet.empty
let all_headers = ref []

let joined sep f l =
  let r = List.map f l in
  String.concat sep
    (List.filter (fun x -> String.compare x "" != 0) r)


let rec main() =
  let include_dir = Filename.concat destdir "include" in
  let src_dir = Filename.concat destdir "src" in
  Unixext.mkdir_rec (Filename.concat include_dir "xen/api") 0o755;
  Unixext.mkdir_rec src_dir 0o755;

  gen_failure_h();
  gen_failure_c();

  let filtered_classes = List.filter (fun x-> not (List.mem x.name ["session"; "debug"; "data_source"])) classes in
  List.iter
    (fun x ->
       (gen_class write_predecl predecl_filename x  include_dir;
        gen_class write_decl    decl_filename    x  include_dir;
        gen_class write_impl    impl_filename    x) src_dir) filtered_classes;

  all_headers := List.map (fun x-> x.name) filtered_classes;

  TypeSet.iter (gen_enum write_enum_decl decl_filename include_dir) !enums;
  TypeSet.iter (gen_enum write_enum_impl impl_filename src_dir) !enums;
  TypeSet.iter (gen_enum write_enum_internal_decl internal_decl_filename include_dir)
    !enums;

  maps := TypeSet.add (Map(String, Int)) !maps;
  maps := TypeSet.add (Map(Int, Int)) !maps;
  maps := TypeSet.add (Map(String, Set (String))) !maps;
  maps := TypeSet.add (Map(String, Map (String, String))) !maps;
  TypeSet.iter (gen_map write_map_decl decl_filename include_dir) !maps;
  TypeSet.iter (gen_map write_map_impl impl_filename src_dir) !maps;

  TypeSet.iter (gen_map write_enum_map_internal_decl internal_decl_filename include_dir) !enum_maps;

  let class_records = filtered_classes |>
                      List.map (fun {name} -> record_typename name) |>
                      List.sort String.compare
  in
  let json1 =`O ["api_class_records", `A (List.map (fun x -> `O ["api_class_record", `String x];) class_records); ] in
  render_file ("xen_internal.mustache", "include/xen_internal.h") json1 templates_dir destdir;

  let sorted_headers = List.sort String.compare (List.map decl_filename !all_headers) in
  let json2 = `O ["api_headers", `A (List.map (fun x -> `O ["api_header", `String x];) sorted_headers); ] in
  render_file ("xen_all.h.mustache", "include/xen/api/xen_all.h") json2 templates_dir destdir


and gen_class f g clas targetdir =
  let out_chan = open_out (Filename.concat targetdir (g clas.name))
  in
  finally (fun () -> f clas out_chan)
    (fun () -> close_out out_chan)


and gen_enum f g targetdir = function
  | Enum(name, contents) ->
    if not (List.mem name !all_headers) then
      all_headers := name::!all_headers;
    let out_chan = open_out (Filename.concat targetdir (g name))
    in
    finally (fun () -> f name contents out_chan)
      (fun () -> close_out out_chan)

  | _ -> assert false


and gen_map f g targetdir = function
  | Map(l, r) ->
    let name = mapname l r in
    if not (List.mem name !all_headers) then
      all_headers := name::!all_headers;
    let out_chan = open_out (Filename.concat targetdir (g name))
    in
    finally (fun () -> f name l r out_chan)
      (fun () -> close_out out_chan)

  | _ -> assert false


and write_predecl {name=classname} out_chan =
  let print format = fprintf out_chan format in
  let protect = protector (classname ^ "_decl") in
  let tn = typename classname in
  let record_tn = record_typename classname in
  let record_opt_tn = record_opt_typename classname in

  print_h_header out_chan protect;

  if classname <> "event" then
    begin
      print "typedef void *%s;\n\n" tn;
      print "%s\n" (predecl_set tn);
    end;
  print "%s\n" (predecl record_tn);
  print "%s\n" (predecl_set record_tn);
  if classname <> "event" then
    begin
      print "%s\n" (predecl record_opt_tn);
      print "%s\n" (predecl_set record_opt_tn);
    end;
  print_h_footer out_chan


and write_decl {name=classname; contents=contents; description=description;
                messages=messages}
    out_chan =
  let print format = fprintf out_chan format in
  let protect = protector classname in
  let tn = typename classname in
  let record_tn = record_typename classname in
  let record_opt_tn = record_opt_typename classname in
  let class_has_refs = true (* !!! *) in
  let needed = ref (StringSet.add (classname ^ "_decl") StringSet.empty) in
  let record = decl_record needed tn record_tn contents in
  let record_opt = decl_record_opt needed tn record_tn record_opt_tn contents in
  let message_decls = decl_messages needed classname
      (List.filter (fun x-> not (classname = "event" && x.msg_name = "from")) messages) in
  let full_stop = if String.endswith "." description then "" else "."
  in

  print_h_header out_chan protect;
  print "%s\n" (hash_includes !needed);

  print "\n\n%s\n\n\n"
    (Helper.comment false
       (sprintf "The %s class.\n\n%s%s" classname description full_stop));

  if classname <> "event" then
    begin
      print "%s\n\n" (decl_free tn (String.lowercase_ascii classname) false "handle");
      print "%s\n" (decl_set tn false);
    end;
  print "%s\n" record;
  if classname <> "event" then
    begin
      print "%s\n" record_opt;
    end;
  print "%s\n\n" (decl_set record_tn class_has_refs);
  if classname <> "event" then
    begin
      print "%s\n\n" (decl_set record_opt_tn true);
    end;
  print "%s\n" message_decls;
  print_h_footer out_chan


and predecl_set tn =
  predecl (tn ^ "_set")


and predecl tn =
  sprintf "struct %s;" tn


and decl_set tn referenced =
  let alloc_com =
    Helper.comment true
      (sprintf "Allocate a %s_set of the given size." tn) in

  sprintf "
typedef struct %s_set
{
    size_t size;
    %s *contents[];
} %s_set;

%s
extern %s_set *
%s_set_alloc(size_t size);

%s
" tn tn tn alloc_com tn tn
    (decl_free (sprintf "%s_set" tn) "*set" referenced "set")


and decl_free tn cn referenced thing =
  let com =
    Helper.comment true
      (sprintf
         "Free the given %s%s.  The given %s must have been allocated by this library."
         tn
         (if referenced then ", and all referenced values" else "") thing) in

  sprintf
    "%s
extern void
%s_free(%s %s);" com tn tn cn


and decl_record needed tn record_tn contents =
  sprintf "
typedef struct %s
{
%s    %s
} %s;

%s
extern %s *
%s_alloc(void);

%s
" record_tn
    (if tn <> "xen_event" then sprintf "    %s handle;\n" tn else "")
    (record_fields contents needed) record_tn
    (Helper.comment true
       (sprintf "Allocate a %s." record_tn))
    record_tn record_tn
    (decl_free record_tn "*record" true "record")


and decl_record_opt needed tn record_tn record_opt_tn contents =
  sprintf "
typedef struct %s
{
    bool is_record;
    union
    {
        %s handle;
        %s *record;
    } u;
} %s;

%s
extern %s *
%s_alloc(void);

%s
" record_opt_tn tn record_tn record_opt_tn
    (Helper.comment true (sprintf "Allocate a %s." record_opt_tn))
    record_opt_tn record_opt_tn
    (decl_free record_opt_tn "*record_opt" true "record_opt")


and record_fields contents needed =
  joined "\n    " (record_field needed "") contents


and record_field needed prefix content =
  match content with
  | Field fr ->
    sprintf "%s%s%s;" (c_type_of_ty needed true fr.ty) prefix
      (fieldname fr.field_name);
  | Namespace (p, c) ->
    joined "\n    " (record_field needed (prefix ^ (fieldname p) ^ "_")) c


and decl_messages needed classname messages =
  joined "\n\n" (decl_message needed classname) messages


and decl_message needed classname message =
  let message_sig = message_signature needed classname message in
  let messageAsyncVersion = decl_message_async needed classname message in
  sprintf "%s\n%sextern %s;\n%s" (get_message_comment message) (get_deprecated_message message) message_sig messageAsyncVersion


and decl_message_async needed classname message =
  if (message.msg_async) then
    (
      let messageSigAsync = message_signature_async needed classname message in
      needed := StringSet.add "task_decl" !needed;
      sprintf "\n%s\n%sextern %s;\n" (get_message_comment message) (get_deprecated_message message) messageSigAsync
    )
  else
    ""


and get_message_comment message =
  let full_stop = if String.endswith "." message.msg_doc then "" else "." in
  Helper.comment true (sprintf "%s%s" message.msg_doc full_stop)


and impl_messages needed classname messages =
  joined "\n\n" (impl_message needed classname) messages


and impl_message needed classname message =
  let message_sig = message_signature needed classname message in
  let param_count = List.length message.msg_params in

  let param_decl, param_call =
    if param_count = 0 then
      ("", "NULL")
    else
      let param_pieces = abstract_params message.msg_params in

      ((sprintf
          "    abstract_value param_values[] =
        {
            %s
        };
" param_pieces), "param_values")
  in

  let result_bits =
    match message.msg_result with
    | Some res -> abstract_result_handling needed classname message.msg_name param_count res
    | None ->
      sprintf
        "    xen_call_(session, \"%s.%s\", %s, %d, NULL, NULL);
    return session->ok;\n" classname message.msg_name param_call param_count
  in

  let messageAsyncImpl=impl_message_async needed classname message in
  sprintf "%s%s\n{\n%s\n%s}\n%s" (get_deprecated_message message) message_sig param_decl result_bits messageAsyncImpl


and impl_message_async needed classname message =
  if (message.msg_async) then
    (
      let messageSigAsync = message_signature_async needed classname message in
      let param_count = List.length message.msg_params in

      let param_decl, param_call =
        if param_count = 0 then
          ("", "NULL")
        else
          let param_pieces = abstract_params message.msg_params in

          ((sprintf
              "    abstract_value param_values[] =
        {
            %s
        };
" param_pieces), "param_values")
      in

      let result_bits = abstract_result_handling_async needed classname message.msg_name param_count in
      sprintf "\n%s%s\n{\n%s\n%s}" (get_deprecated_message message) messageSigAsync param_decl result_bits
    )
  else
    ""


and abstract_params params =
  joined ",\n            " abstract_param params


and abstract_param p =
  let ab_typ = abstract_type false p.param_type in
  sprintf "{ .type = &%s,
              .u.%s_val = %s }" ab_typ (abstract_member p.param_type)
    (abstract_param_conv p.param_name p.param_type)


and abstract_param_conv name = function
  | Set _
  | Map _ ->
    sprintf "(arbitrary_set *)%s" (paramname name)
  | Ref "session" -> sprintf "%s->session_id" (paramname name)
  | _ -> paramname name

and abstract_member = function
    String
  | Ref _ -> "string"
  | Enum _ -> "enum"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "datetime"
  | Set _ -> "set"
  | Map _ -> "set"
  | Record _ -> "struct"


and abstract_result_handling needed classname msg_name param_count = function
    typ, _ ->
    let call =
      if param_count = 0 then
        sprintf "xen_call_(session, \"%s.%s\", NULL, 0, &result_type, result);" classname msg_name
      else
        sprintf "XEN_CALL_(\"%s.%s\");" classname msg_name
    in

    match typ with
      String
    | Ref _
    | Int
    | Float
    | Bool
    | DateTime
    | Set _
    | Map _ ->
      sprintf
        "%s

%s    %s
    return session->ok;
" (abstract_result_type typ) (initialiser_of_ty typ) call

    | Record n ->
      let record_tn = record_typename n in
      sprintf
        "    abstract_type result_type = %s_abstract_type_;

%s    %s

    if (session->ok)
    {
       (*result)->handle = xen_strdup_((*result)->uuid);
    }

    return session->ok;
" record_tn (initialiser_of_ty (Record n)) call

    | Enum(e, _) ->
      sprintf
        "%s
    %s
    return session->ok;
" (abstract_result_type typ) call


and abstract_result_handling_async needed classname msg_name param_count =
  let call =
    if param_count = 0 then
      sprintf "xen_call_(session, \"Async.%s.%s\", NULL, 0, &result_type, result);" classname msg_name
    else
      sprintf "XEN_CALL_(\"Async.%s.%s\");" classname msg_name
  in
  sprintf
    "    abstract_type result_type = abstract_type_string;

    *result = NULL;
    %s
    return session->ok;\n" call

and abstract_record_field classname prefix prefix_caps content =
  match content with
    Field fr ->
    let fn = fieldname fr.field_name in
    sprintf "{ .key = \"%s%s\",
          .type = &%s,
          .offset = offsetof(%s, %s%s) }"
      prefix_caps fr.field_name (abstract_type true fr.ty)
      (record_typename classname) prefix fn;
  | Namespace (p, c) ->
    joined ",\n        " (abstract_record_field classname
                            (prefix ^ (fieldname p) ^ "_")
                            (prefix_caps ^ p ^ "_")) c

and abstract_result_type typ =
  let ab_typ = abstract_type false typ in
  sprintf "    abstract_type result_type = %s;" ab_typ


and abstract_type record = function
  | String      -> "abstract_type_string"
  | Enum(n, _)  ->
    sprintf "%s_abstract_type_" (typename n)
  | Ref _       ->
    if record then
      "abstract_type_ref"
    else
      "abstract_type_string"
  | Int         -> "abstract_type_int"
  | Float       -> "abstract_type_float"
  | Bool        -> "abstract_type_bool"
  | DateTime    -> "abstract_type_datetime"
  | Set (Enum(n, _)) ->
    sprintf "%s_set_abstract_type_" (typename n)
  | Set (Record n) -> sprintf "%s_set_abstract_type_" (record_typename n)
  | Set memtype -> (abstract_type record memtype) ^ "_set"
  | Map(Ref(_), Ref(_)) ->
    if record then "abstract_type_string_ref_map" else "abstract_type_string_string_map"
  | Map(Ref(_), r) -> sprintf "abstract_type_string_%s_map" (name_of_ty r)
  | Map(l, Ref(_)) ->
    if record then
      sprintf "abstract_type_%s_ref_map" (name_of_ty l)
    else
      sprintf "abstract_type_%s_string_map" (name_of_ty l)
  | Map((Enum(_,_) as l), r) -> (mapname l r) ^ "_abstract_type_"
  | Map(l, (Enum(_,_) as r)) -> (mapname l r) ^ "_abstract_type_"
  | Map(l, r) -> sprintf "abstract_type_" ^ (mapname l r)

  | Record n -> sprintf "%s_abstract_type_" (record_typename n)

and get_deprecated_message message =
  let deprecatedMessage = get_deprecated_info_message message in
  if deprecatedMessage = "" then sprintf "" else sprintf "/* " ^ deprecatedMessage ^ " */\n"


and message_signature needed classname message =
  let front =
    {param_type=Ref "session"; param_name="session"; param_doc=""; param_release=message.msg_release; param_default = None} ::
    match message.msg_result with
      Some res -> [{param_type=fst res; param_name="*result"; param_doc=""; param_release=message.msg_release; param_default = None}]
    | None -> []
  in
  let params = joined ", " (param needed) (front @ message.msg_params) in
  sprintf "bool\n%s(%s)" (messagename classname message.msg_name) params


and message_signature_async needed classname message =
  let sessionParam = {param_type=Ref "session"; param_name="session"; param_doc=""; param_release=message.msg_release; param_default = None} in
  let taskParam= {param_type=Ref "task"; param_name="*result"; param_doc=""; param_release=message.msg_release; param_default = None} in
  let params = joined ", " (param needed) (sessionParam :: (taskParam :: message.msg_params)) in
  sprintf "bool\n%s(%s)" (messagename_async classname message.msg_name) params


and param needed p =
  let t = p.param_type in
  let n = p.param_name in
  sprintf "%s%s" (c_type_of_ty needed false t) (paramname n)


and hash_includes needed =
  String.concat "\n"
    (List.sort
       String.compare
       (List.filter
          (function s -> s <> "")
          (List.map hash_include ("common" :: StringSet.elements needed))))


and hash_include n =
  if String.endswith "internal" n
  then
    sprintf "#include \"%s\"" (decl_filename n)
  else if n = "session" then
    ""
  else
    sprintf "#include <%s>" (decl_filename n)


and write_enum_decl name contents out_chan =
  let print format = fprintf out_chan format in
  let protect = protector name in
  let tn = typename name in

  print_h_header out_chan protect;

  print "
%s


enum %s
{
%s
};


typedef struct %s_set
{
    size_t size;
    enum %s contents[];
} %s_set;

%s
extern %s_set *
%s_set_alloc(size_t size);

%s


%s
extern const char *
%s_to_string(enum %s val);


%s
extern enum %s
%s_from_string(xen_session *session, const char *str);

" (hash_include "common") tn
    (joined ",\n\n" (enum_entry name)
       (contents @
        [("undefined", "Unknown to this version of the bindings.")]))
    tn tn tn
    (Helper.comment true
       (sprintf "Allocate a %s_set of the given size." tn))
    tn tn
    (decl_free (sprintf "%s_set" tn) "*set" false "set")
    (Helper.comment true
       "Return the name corresponding to the given code.  This string must not be modified or freed.")
    tn tn
    (Helper.comment true
       "Return the correct code for the given string, or set the session object to failure and return an undefined value if the given string does not match a known code.")
    tn tn;

  print_h_footer out_chan


and enum_entry enum_name = function
    (n, c) ->
    sprintf "%s\n    XEN_%s_%s" (Helper.comment true ~indent:4 c)
      (String.uppercase_ascii enum_name) (String.replace "-" "_" (String.uppercase_ascii n))


and write_enum_impl name contents out_chan =
  let print format = fprintf out_chan format in
  let tn = typename name in

  print
    "%s

#include <string.h>

%s
%s
%s


/*
 * Maintain this in the same order as the enum declaration!
 */
static const char *lookup_table[] =
{
%s
};


extern %s_set *
%s_set_alloc(size_t size)
{
    return calloc(1, sizeof(%s_set) +
                  size * sizeof(enum %s));
}


extern void
%s_set_free(%s_set *set)
{
    free(set);
}


const char *
%s_to_string(enum %s val)
{
    return lookup_table[val];
}


extern enum %s
%s_from_string(xen_session *session, const char *str)
{
    (void)session;
    return ENUM_LOOKUP(str, lookup_table);
}


const abstract_type %s_abstract_type_ =
    {
        .typename = ENUM,
        .enum_marshaller =
             (const char *(*)(int))&%s_to_string,
        .enum_demarshaller =
             (int (*)(xen_session *, const char *))&%s_from_string
    };


" Licence.bsd_two_clause (hash_include "internal") (hash_include name)
    (hash_include (name ^ "_internal"))
    (enum_lookup_entries
       (contents @ [("undefined", "")]))
    tn tn tn tn
    tn tn
    tn tn tn tn
    tn tn tn;

  if name <> "event_operation"
  then
    print
      "const abstract_type %s_set_abstract_type_ =
    {
        .typename = SET,
        .child = &%s_abstract_type_
    };


" tn tn


and enum_lookup_entries contents =
  joined ",\n" enum_lookup_entry contents


and enum_lookup_entry = function
    n, _ ->
    sprintf "    \"%s\"" n


and write_enum_internal_decl name contents out_chan =
  let print format = fprintf out_chan format in
  let protect = protector (sprintf "%s_internal" name) in
  let tn = typename name in

  let set_abstract_type =
    (if name = "event_operations"
     then
       ""
     else
       sprintf "extern const abstract_type %s_set_abstract_type_;\n" tn)
  in

  print
    "%s


%s


#ifndef %s
#define %s


%s


extern const abstract_type %s_abstract_type_;
%s

#endif
" Licence.bsd_two_clause
    (Helper.comment false (sprintf "Declarations of the abstract types used during demarshalling of enum %s.  Internal to this library -- do not use from outside." tn))
    protect protect
    (hash_include "internal") tn
    set_abstract_type


and write_map_decl name l r out_chan =
  let print format = fprintf out_chan format in
  let tn = typename name in
  let protect = protector name in
  let needed = ref StringSet.empty in
  let alloc_com =
    Helper.comment true
      (sprintf "Allocate a %s of the given size." tn) in

  print_h_header out_chan protect;
  print "
%s%s%s


typedef struct %s_contents
{
  %skey;
  %sval;
} %s_contents;


typedef struct %s
{
    size_t size;
    %s_contents contents[];
} %s;

%s
extern %s *
%s_alloc(size_t size);

%s

" (hash_include "common") (hash_include_enum l) (hash_include_enum r)
    tn
    (c_type_of_ty needed false l)
    (c_type_of_ty needed true r) tn tn tn tn
    alloc_com tn tn (decl_free tn "*map" true "map");
  print_h_footer out_chan

and write_map_impl name l r out_chan =
  let print format = fprintf out_chan format in
  let tn = typename name in
  let l_free_impl = free_impl "map->contents[i].key" false l in
  let r_free_impl = free_impl "map->contents[i].val" true r in
  let needed = ref StringSet.empty in
  find_needed'' needed l;
  find_needed'' needed r;
  needed := StringSet.add "internal" !needed;
  needed := StringSet.add name !needed;
  begin
    match r with
      Set(String) ->
      needed := StringSet.add ("string_set") !needed
    | _ -> ()
  end;

  print
    "%s


%s


%s *
%s_alloc(size_t size)
{
    %s *result = calloc(1, sizeof(%s) +
    %s                  size * sizeof(struct %s_contents));
    result->size = size;
    return result;
}


void
%s_free(%s *map)
{
" Licence.bsd_two_clause (hash_includes !needed) tn tn tn tn
    (String.make (String.length tn) ' ') tn tn tn;

  if (String.compare l_free_impl "" != 0 ||
      String.compare r_free_impl "" != 0) then
    print
      "    if (map == NULL)
    {
        return;
    }

    size_t n = map->size;
    for (size_t i = 0; i < n; i++)
    {
        %s
        %s
    }

" l_free_impl r_free_impl;

  print
    "    free(map);
}
";

  begin
    match l, r with
      (Enum(_, _), _) -> gen_enum_map_abstract_type print name l r
    | (_, Enum(_, _)) -> gen_enum_map_abstract_type print name l r
    | _ -> ()
  end


and gen_enum_map_abstract_type print name l r =
  let tn = mapname l r in
  print "

static const struct_member %s_struct_members[] =
    {
        { .type = &%s,
          .offset = offsetof(xen_%s_contents, key) },
        { .type = &%s,
          .offset = offsetof(xen_%s_contents, val) },
    };

const abstract_type %s_abstract_type_ =
    {
       .typename = MAP,
       .struct_size = sizeof(%s_struct_members),
       .member_count =
           sizeof(%s_struct_members) / sizeof(struct_member),
       .members = %s_struct_members
    };
" tn
    (abstract_type false l) tn
    (abstract_type false r) tn
    tn tn tn tn


and write_enum_map_internal_decl name l r out_chan =
  let print format = fprintf out_chan format in
  let protect = protector (sprintf "%s_internal" name) in

  print_h_header out_chan protect;
  print "
extern const abstract_type %s_abstract_type_;

" (mapname l r);
  print_h_footer out_chan


and hash_include_enum = function
    Enum(x, _) ->
    "\n" ^ hash_include x
  | _ ->
    ""

and gen_failure_h () =
  let protect = protector "api_failure" in
  let out_chan = open_out (Filename.concat destdir "include/xen/api/xen_api_failure.h")
  in
  finally (fun () ->
      print_h_header out_chan protect;
      gen_failure_enum out_chan;
      gen_failure_funcs out_chan;
      print_h_footer out_chan)
    (fun () -> close_out out_chan)

and gen_failure_enum out_chan =
  let print format = fprintf out_chan format in
  print "
enum xen_api_failure
{
%s
};


" (String.concat ",\n\n" (failure_enum_entries()))


and failure_enum_entries() =
  let r = Hashtbl.fold failure_enum_entry Datamodel.errors [] in
  let r = List.sort (fun (x, _) (y, _) -> String.compare y x) r in
  let r = failure_enum_entry "UNDEFINED"
      { err_doc = "Unknown to this version of the bindings.";
        err_params = [];
        err_name = "UNDEFINED"; } r
  in
  (List.map (fun (x, y) -> y) (List.rev r))


and failure_enum_entry name err acc =
  (name, sprintf
     "%s
    %s"
     (Helper.comment true ~indent:4 err.Datamodel_types.err_doc)
     (failure_enum name)) :: acc


and gen_failure_funcs out_chan =
  let print format = fprintf out_chan format in
  print "%s
extern const char *
xen_api_failure_to_string(enum xen_api_failure val);


%s
extern enum xen_api_failure
xen_api_failure_from_string(const char *str);

"
    (Helper.comment true
       "Return the name corresponding to the given code.  This string must not be modified or freed.")
    (Helper.comment true
       "Return the correct code for the given string, or UNDEFINED if the given string does not match a known code.")


and gen_failure_c () =
  let out_chan = open_out (Filename.concat destdir "src/xen_api_failure.c") in
  let print format = fprintf out_chan format
  in
  finally (fun () ->
      print "%s

#include \"xen_internal.h\"
#include <xen/api/xen_api_failure.h>


/*
 * Maintain this in the same order as the enum declaration!
 */
static const char *lookup_table[] =
{
    %s
};


const char *
xen_api_failure_to_string(enum xen_api_failure val)
{
    return lookup_table[val];
}


extern enum xen_api_failure
xen_api_failure_from_string(const char *str)
{
    return ENUM_LOOKUP(str, lookup_table);
}


" Licence.bsd_two_clause
        (String.concat ",\n    " (failure_lookup_entries out_chan)))
    (fun () -> close_out out_chan)

and failure_lookup_entries out_chan =
  List.sort String.compare
    (Hashtbl.fold failure_lookup_entry Datamodel.errors [])

and failure_lookup_entry name _ acc =
  (sprintf "\"%s\"" name) :: acc

and failure_enum name =
  "XEN_API_FAILURE_" ^ (String.uppercase_ascii name)


and write_impl {name=classname; contents=contents; messages=messages} out_chan =
  let is_event = (classname = "event") in
  let print format = fprintf out_chan format in
  let needed = ref StringSet.empty in
  let tn = typename classname in
  let record_tn = record_typename classname in
  let record_opt_tn = record_opt_typename classname in
  let msgs =
    impl_messages needed classname
      (List.filter (fun x-> not (classname = "event" && x.msg_name = "from")) messages)
  in
  let record_free_handle =
    if classname = "event" then "" else "    free(record->handle);\n" in
  let record_free_impls =
    joined "\n    " (record_free_impl "record->") contents in
  let filtered_record_fields =
    let not_obj_uuid x = match x with | Field r when r.field_name = "obj_uuid" -> false | _ -> true in
    if is_event then List.filter not_obj_uuid contents else contents in
  let record_fields =
    joined ",\n        " (abstract_record_field classname "" "") filtered_record_fields in
  let needed = ref StringSet.empty in
  find_needed needed messages;
  needed := StringSet.add "internal" !needed;
  needed := StringSet.add classname !needed;

  let getAllRecordsExists = List.exists (fun x -> x.msg_name = "get_all_records") messages in
  let mappingName = sprintf "%s_%s" tn record_tn in

  let free_block =
    String.concat "\n"
      ((if is_event then [] else
          [sprintf "XEN_FREE(%s)" tn;
           sprintf "XEN_SET_ALLOC_FREE(%s)" tn]) @
       [sprintf "XEN_ALLOC(%s)" record_tn;
        sprintf "XEN_SET_ALLOC_FREE(%s)" record_tn] @
       (if is_event then [] else
          [sprintf "XEN_ALLOC(%s)" record_opt_tn;
           sprintf "XEN_RECORD_OPT_FREE(%s)" tn;
           sprintf "XEN_SET_ALLOC_FREE(%s)" record_opt_tn]))
  in

  print
    "%s


#include <stddef.h>
#include <stdlib.h>

%s


%s


" Licence.bsd_two_clause (hash_includes !needed) free_block;


  print
    "static const struct_member %s_struct_members[] =
    {
        %s
    };

const abstract_type %s_abstract_type_ =
    {
       .typename = STRUCT,
       .struct_size = sizeof(%s),
       .member_count =
           sizeof(%s_struct_members) / sizeof(struct_member),
       .members = %s_struct_members
    };


" record_tn record_fields record_tn record_tn record_tn record_tn;

  print
    "const abstract_type %s_set_abstract_type_ =
    {
       .typename = SET,
        .child = &%s_abstract_type_
    };\n\n\n" record_tn record_tn;

  if getAllRecordsExists then
    print
      "static const struct struct_member %s_members[] =
{
    {
        .type = &abstract_type_string,
        .offset = offsetof(%s_map_contents, key)
    },
    {
        .type = &%s_abstract_type_,
        .offset = offsetof(%s_map_contents, val)
    }
};

const abstract_type abstract_type_string_%s_map =
{
    .typename = MAP,
    .struct_size = sizeof(%s_map_contents),
    .members = %s_members
};\n\n\n" mappingName mappingName record_tn mappingName record_tn mappingName mappingName;

  print
    "void
%s_free(%s *record)
{
    if (record == NULL)
    {
        return;
    }
%s    %s
    free(record);
}\n\n\n" record_tn record_tn record_free_handle record_free_impls;

  print "%s\n" msgs


and find_needed needed messages =
  List.iter (find_needed' needed) messages


and find_needed' needed message =
  List.iter (fun p -> find_needed'' needed p.param_type) message.msg_params;
  match message.msg_result with
    Some (x, _) ->
    find_needed'' needed x
  | None ->
    ()


and find_needed'' needed = function
  | String
  | Int
  | Float
  | Bool
  | DateTime -> ()
  | Enum (n, _) ->
    needed := StringSet.add (n ^ "_internal") !needed
  | Ref n ->
    needed := StringSet.add n !needed
  | Set(Ref n) ->
    needed := StringSet.add n !needed
  | Set(Enum (e, _)) ->
    needed := StringSet.add e !needed;
    needed := StringSet.add (e ^ "_internal") !needed
  | Set(Record "event") ->
    needed := StringSet.add ("event_operation_internal") !needed
  | Map(l, r) ->
    let n = mapname l r in
    needed := StringSet.add n !needed;
    needed := add_enum_map_internal !needed l r;
    needed := add_enum_internal !needed l;
    needed := add_enum_internal !needed r
  | _ ->
    ()

and record_free_impl prefix = function
  | Field fr         -> free_impl (prefix ^ (fieldname fr.field_name)) true fr.ty
  | Namespace (p, c) -> joined "\n    " (record_free_impl (prefix ^ (fieldname p) ^ "_")) c


and free_impl val_name record = function
  | String           -> sprintf "free(%s);" val_name
  | Int
  | Float
  | Bool
  | DateTime
  | Enum (_, _)      -> ""
  | Ref n            -> sprintf "%s_free(%s);" (if record then record_opt_typename n else typename n) val_name
  | Set(Ref n)       -> sprintf "%s_opt_set_free(%s);" (record_typename n) val_name
  | Set(Enum (e, _)) -> sprintf "%s_set_free(%s);" (typename e) val_name
  | Set(String)      -> sprintf "xen_string_set_free(%s);" val_name
  | Map(l, r)        -> let n = mapname l r in
    sprintf "%s_free(%s);" (typename n) val_name
  | Record x         -> sprintf "%s_free(%s);" (record_typename x) val_name
  | Set(Int)         -> sprintf "xen_int_set_free(%s);" val_name
  | _                -> "DONT_KNOW"


and add_enum_internal needed = function
  | Enum(x, _) -> StringSet.add (x ^ "_internal") needed
  | _          -> needed


and add_enum_map_internal needed l r =
  match (l, r) with
    (Enum(_, _), _) -> StringSet.add ((mapname l r) ^ "_internal") needed
  | (_, Enum(_, _)) -> StringSet.add ((mapname l r) ^ "_internal") needed
  | _ -> needed


and c_type_of_ty needed record = function
  | String              -> "char *"
  | Int                 -> "int64_t "
  | Float               -> "double "
  | Bool                -> "bool "
  | DateTime            -> "time_t "
  | Ref "session"       -> "xen_session *"
  | Ref name            ->
    needed := StringSet.add (name ^ "_decl") !needed;
    if record then
      sprintf "struct %s *" (record_opt_typename name)
    else
      sprintf "%s " (typename name)
  | Enum(name, cs) as x ->
    needed := StringSet.add name !needed;
    enums := TypeSet.add x !enums;
    c_type_of_enum name
  | Set (Ref name) ->
    needed := StringSet.add (name ^ "_decl") !needed;
    if record then
      sprintf "struct %s_set *" (record_opt_typename name)
    else
      sprintf "struct %s_set *" (typename name)
  | Set (Enum (e, _) as x) ->
    let enum_typename = typename e in
    needed := StringSet.add e !needed;
    enums := TypeSet.add x !enums;
    sprintf "struct %s_set *" enum_typename
  | Set(String) ->
    needed := StringSet.add "string_set" !needed;
    "struct xen_string_set *"
  | Set (Record n) ->
    needed := StringSet.add (n ^ "_decl") !needed;
    sprintf "struct %s_set *" (record_typename n)
  | Set (Int)            ->
    needed := StringSet.add "int_set" !needed;
    "struct xen_int_set *"
  | Map(l, r) as x ->
    let n = mapname l r in
    needed := StringSet.add n !needed;
    maps := TypeSet.add x !maps;
    begin
      match (l, r) with
        (Enum(_, _), _) -> enum_maps := TypeSet.add x !enum_maps
      | (_, Enum(_, _)) -> enum_maps := TypeSet.add x !enum_maps
      | _ -> ()
    end;
    sprintf "%s *" (typename n)
  | Record n -> if record then
      sprintf "struct %s *" (record_typename n)
    else
      sprintf "%s *" (record_typename n)
  | _ -> assert false


and c_type_of_enum name =
  sprintf "enum %s " (typename name)


and initialiser_of_ty = function
  | String
  | Ref _
  | Set _
  | Map _
  | Record _ -> "    *result = NULL;\n"
  | _ -> ""


and mapname l r =
  sprintf "%s_%s_map" (name_of_ty l) (name_of_ty r)


and name_of_ty = function
  | String   -> "string"
  | Int      -> "int"
  | Float    -> "float"
  | Bool     -> "bool"
  | DateTime -> "datetime"
  | Enum(x, _) -> x
  | Set(x)   -> sprintf "%s_set" (name_of_ty x)
  | Ref(x)   -> x
  | Map(l,r) -> sprintf "%s_%s_map" (name_of_ty l) (name_of_ty r)
  | Record n -> sprintf "%s" (record_typename n)

and decl_filename name =
  let dir = (if String.endswith "internal" name then "" else "xen/api/") in
  sprintf "%sxen_%s.h" dir (String.lowercase_ascii name)


and predecl_filename name =
  sprintf "xen/api/xen_%s_decl.h" (String.lowercase_ascii name)


and internal_decl_filename name =
  sprintf "xen_%s_internal.h" (String.lowercase_ascii name)


and impl_filename name =
  sprintf "xen_%s.c" (String.lowercase_ascii name)


and internal_impl_filename name =
  sprintf "xen_%s_internal.c" (String.lowercase_ascii name)


and protector classname =
  sprintf "XEN_%s_H" (String.uppercase_ascii classname)


and typename classname =
  sprintf "xen_%s" (String.lowercase_ascii classname)


and variablename classname =
  sprintf "%s" (String.lowercase_ascii classname)


and record_typename classname =
  sprintf "%s_record" (typename classname)


and record_opt_typename classname =
  sprintf "%s_record_opt" (typename classname)


and messagename classname name =
  sprintf "xen_%s_%s" (String.lowercase_ascii classname)
    (String.lowercase_ascii name)


and messagename_async classname name =
  sprintf "xen_%s_%s_async" (String.lowercase_ascii classname)
    (String.lowercase_ascii name)

and keyword_map name =
  let keywords = ["class","XEN_CLAZZ"; "public", "pubblic"] in
  if List.mem_assoc name keywords then List.assoc name keywords else name

and paramname name =
  keyword_map (String.lowercase_ascii name)


and fieldname name =
  keyword_map (String.lowercase_ascii name)


and print_h_header out_chan protect =
  let print format = fprintf out_chan format in
  print "%s\n\n" Licence.bsd_two_clause;
  print "#ifndef %s\n" protect;
  print "#define %s\n\n" protect;

and print_h_footer out_chan =
  fprintf out_chan "\n#endif\n"

and populate_version () =
  List.iter (fun x -> render_file x json_releases templates_dir destdir) [
    "Makefile.mustache",          "Makefile";
    "xen_api_version.h.mustache", "include/xen/api/xen_api_version.h";
    "xen_api_version.c.mustache", "src/xen_api_version.c";
  ]

let _ =
  main ();
  populate_version ()
