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

open Datamodel
open Datamodel_utils


(** Output stuff as Perl *)

let rec all system dirname = 
  List.iter (output_module dirname) system


and output_module dirname obj =
  let filename =
    Filename.concat dirname (sprintf "%s.pm" obj.name) in
  let chan = open_out filename in

    try
      output_module' chan obj;
      close_out chan
    with e ->
      close_out chan;
      raise e


and output_module' chan obj =
  let methods = obj.messages @ (accessors obj.contents) in

    fprintf chan "#
# Automatically generated from datamodel specification and IDL.
# Do not edit.
#

package Xen::%s;

use 5.008004;
use strict;
use warnings;

require Exporter;

require RPC::XML;
require RPC::XML::Client;

our @ISA = qw(Exporter);

our %%EXPORT_TAGS = ( 'all' => [ qw(
%s
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw();

our $VERSION = '0.01';

%s

1;
__END__

=head1 NAME

Xen::%s - %s

=head1 SYNOPSIS

use Xen::%s;

=head1 DESCRIPTION

%s

=head2 EXPORT

None by default.

=head1 SEE ALSO

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2006 XenSource Inc.  All rights reserved.

=cut

" obj.name (method_names methods) (method_bodies obj.name methods) obj.name
      obj.description obj.name obj.description


and accessors contents =
  List.fold_right accessors' contents []


and accessors' content acc =
  match content with
      Field(_, _, RW, name, ty, desc) ->
        (getter name ty desc) :: (setter name ty desc) :: acc

    | Field(_, _, _, name, ty, desc) ->
        (getter name ty desc) :: acc
    | _ ->
        acc


and getter name ty desc =
  call
    ~name:(sprintf "get_%s" name)
    ~doc:desc
    ~flags:[`Session]
    ~result:(ty, name)
    ~params:[Unit, "", ""]
    ()


and setter name ty desc =
  call
    ~name:(sprintf "set_%s" name)
    ~doc:desc
    ~flags:[`Session]
    ~result:void
    ~params:[ty, "val", ""]
    ()


and method_names methods =
  String.concat " " (List.map method_name methods)


and method_name m =
  m.msg_name


and method_bodies group methods =
  String.concat "" (List.map (method_body group) methods)


and method_body group m =
  let methodname = method_name m in
  let p =
    match m.msg_params with
        [(Unit, _, _)] -> []
      | _              -> m.msg_params
  in

    (* Note that the \123 below is a left-brace.  Tuareg mode gets confused
       if you put the brace in there explicitly. *)
    sprintf "
# %s %s
sub %s
\123
    my (%s) = @_;
%s
}

" methodname (comment p) methodname (params "$url" p)
      (message group m.msg_name p)


and comment p =
  String.concat ", "
    (List.map
       (fun (ty, label, _) ->
          (sprintf "%s %s" (perl_type_of_ty ty) label))
       ((String, "url", "") :: p))


and message group methodname p =
  sprintf "
    my $client = RPC::XML::Client->new($url);
    my $response = $client->send_request(%s);

    if (ref $response) {
      return $response->value;
    }
    else {
      die $response;
    }
" (params (sprintf "'%s.%s'" group methodname) p)


and params prefix params =
  String.concat ", "
    (prefix ::
       (List.map
          (fun (ty, label, _) -> (sprintf "$%s" label))
          params))


and perl_type_of_ty = function
    String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Uuid -> "string"
  | DateTime -> "datetime"
  | Enum (name, things) -> name
  | List x -> (perl_type_of_ty x) ^ " list"
  | Map (a, b) -> "" ^ (perl_type_of_ty a) ^ " " ^ (perl_type_of_ty b) ^ " map"
  | Ref obj -> obj ^ " ref"

  | StringAlias x -> x
  | XML -> "string"
  | Unit -> "void"


and camelCase s firstCaps =
  let n = String.length s in
  let caps = ref firstCaps in
  let result = ref "" in
  let f c =
    if c = '_' then
      caps := true
    else if !caps then
      (result := sprintf "%s%c" !result (Char.uppercase c);
       caps := false)
    else
      result := sprintf "%s%c" !result c
  in
    String.iter f s;
    !result
