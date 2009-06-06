(* Very minimal lexer for SQL statements for doing a db upgrade *)

{
open Sqlparse

let create_hashtable size init =
  let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

let get         = Lexing.lexeme
let getchar     = Lexing.lexeme_char


let keyword_table = 
    create_hashtable 8 [
      ("CREATE", CREATE);
      ("TABLE", TABLE);
      ("INSERT", INSERT);
      ("INTO", INTO);
      ("VALUES", VALUES);
      ("COMMIT", COMMIT);
      ("BEGIN", BEGIN);
      ("TRANSACTION", TRANSACTION);
     ]

exception End_of_file
exception Error of string
}

let id = ['a'-'z' 'A'-'Z' '_' ]['a'-'z' 'A'-'Z' '0'-'9' '_']*
let str1 = '\'' ([^'\'' '\\'] | '\\' ('\\' | '\''))* '\''

rule token = parse (* main lexer *)
  | eof { raise End_of_file }
  | id as word {
      try
	let token = Hashtbl.find keyword_table word in
	token
      with Not_found ->
	ID word
    }
  | "\'" {STRING1 (string lexbuf (Buffer.create 80))}
  | "\"" {STRING2 (string2 lexbuf (Buffer.create 80))}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "," {COMMA}
  | ";" {SEMICOLON}
  | [' ' '\t' '\n'] { token lexbuf }


and string = parse (* lexer for strings *)
    eof     { fun buf -> raise (Error "EOF in string") } 
  | '\\' _  { fun buf -> 
      let toadd = 
	match getchar lexbuf 1 with
	    '\'' -> "'"
	  | c -> Printf.sprintf "\\%c" c 
      in
      Buffer.add_string buf toadd;
      string lexbuf buf}                 
  | "'"     { fun buf -> Buffer.contents buf } (* return string *)
  | _ as c  { fun buf -> string lexbuf (Buffer.add_char buf c; buf)}

and string2 = parse (* lexer for strings *)
    eof     { fun buf -> raise (Error "EOF in string") } 
  | '\\' _  { fun buf -> 
      let toadd = 
	match getchar lexbuf 1 with
	    '"' -> "\""
	  | c -> Printf.sprintf "\\%c" c 
      in
      Buffer.add_string buf toadd;
      string2 lexbuf buf}                 
  | "\""    { fun buf -> Buffer.contents buf } (* return string *)
  | _ as c  { fun buf -> string2 lexbuf (Buffer.add_char buf c; buf)}
 
{

}
