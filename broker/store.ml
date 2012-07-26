
(* Merge this with xenstore *)

module String = struct
  include String

let rec split ?limit:(limit=(-1)) c s =
  let i = try String.index s c with Not_found -> -1 in
  let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
  if i = -1 || nlimit = 0 then
    [ s ]
  else
    let a = String.sub s 0 i
    and b = String.sub s (i + 1) (String.length s - i - 1) in
    a :: (split ~limit: nlimit c b)

let fold_left f accu string =
        let accu = ref accu in
        for i = 0 to length string - 1 do
                accu := f !accu string.[i]
        done;
        !accu

end

module Node = struct

type 'a t = {
  name: string;
  value: 'a;
  children: 'a t list;
}

let create _name _value =
        { name = _name; value = _value; children = []; }

let exists node childname =
        List.exists (fun n -> n.name = childname) node.children

let find node childname =
        List.find (fun n -> n.name = childname) node.children

let replace_child node child nchild =
        (* this is the on-steroid version of the filter one-replace one *)
        let rec replace_one_in_list l =
                match l with
                | []                               -> []
                | h :: tl when h.name = child.name -> nchild :: tl
                | h :: tl                          -> h :: replace_one_in_list tl
                in
        { node with children = (replace_one_in_list node.children) }

end

module Path = struct

exception Invalid_path

exception Lookup_Doesnt_exist of string

exception Doesnt_exist

exception Already_exist

(* represent a path in a store.
 * [] -> "/"
 * [ "local"; "domain"; "1" ] -> "/local/domain/1"
 *)
type t = string list

let char_is_valid c =
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c = '_' || c = '-' || c = '@'

let name_is_valid name =
        name <> "" && String.fold_left (fun accu c -> accu && char_is_valid c) true name

let is_valid path =
        List.for_all name_is_valid path

let of_string s =
        if s.[0] = '@'
        then [s]
        else if s = "/"
        then []
        else match String.split '/' s with
                | "" :: path when is_valid path -> path
                | _ -> raise Invalid_path

let path_complete path connection_path =
        if String.get path 0 <> '/' then
                connection_path ^ path
        else
                path

let path_validate path connection_path =
        if String.length path = 0 || String.length path > 1024 then
                raise Invalid_path
        else
                let cpath = path_complete path connection_path in
                if String.get cpath 0 <> '/' then
                        raise Invalid_path
                else
                        cpath


let create path connection_path =
        of_string (path_validate path connection_path)

let to_string t =
        "/" ^ (String.concat "/" t)

let to_string_list x = x

let get_parent t =
        if t = [] then [] else List.rev (List.tl (List.rev t))

let list_tl_multi n l =
        let rec do_tl i x =
                if i = 0 then x else do_tl (i - 1) (List.tl x)
                in
        do_tl n l

let get_hierarchy path =
        let l = List.length path in
        let revpath = List.rev path in
        let rec sub i =
                let x = List.rev (list_tl_multi (l - i) revpath) in
                if i = l then [ x ] else x :: sub (i + 1)
                in
        sub 0

let get_common_prefix p1 p2 =
        let rec compare l1 l2 =
                match l1, l2 with
                | h1 :: tl1, h2 :: tl2 ->
                        if h1 = h2 then h1 :: (compare tl1 tl2) else []
                | _, [] | [], _ ->
                        (* if l1 or l2 is empty, we found the equal part already *)
                        []
                in
        compare p1 p2

let rec lookup_modify node path fct =
        match path with
        | []      -> raise Invalid_path
        | h :: [] -> fct node h
        | h :: l  ->
                let (n, c) =
                        if not (Node.exists node h) then
                                raise (Lookup_Doesnt_exist h)
                        else
                                (node, Node.find node h) in
                let nc = lookup_modify c l fct in
                Node.replace_child n c nc

let apply_modify rnode path fct =
        lookup_modify rnode path fct

let rec lookup_get node path =
        match path with
        | []      -> raise (Invalid_path)
        | h :: [] ->
                (try
                        Node.find node h
                with Not_found ->
                        raise Doesnt_exist)
        | h :: l  -> let cnode = Node.find node h in lookup_get cnode l

let get_node rnode path =
        if path = [] then
                Some rnode
        else (
                try Some (lookup_get rnode path) with Doesnt_exist -> None
        )

(* get the deepest existing node for this path *)
let rec get_deepest_existing_node node = function
        | [] -> node
        | h :: t ->
                try get_deepest_existing_node (Node.find node h) t 
                with Not_found -> node

(* read | ls | getperms use this *)
let rec lookup node path fct =
        match path with
        | []      -> raise (Invalid_path)
        | h :: [] -> fct node h
        | h :: l  -> let cnode = Node.find node h in lookup cnode l fct

let apply rnode path fct =
        lookup rnode path fct
end

