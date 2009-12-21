{
open Lvmconfigparser
}

let digit = ['0'-'9']
let name = ['a'-'z' 'A'-'Z' '0'-'9' '.' '_' '+'] ['a'-'z' 'A'-'Z' '0'-'9' '.' '_' '+' '-']*
 
rule lvmtok = parse
  | '#' [^ '\n']* '\n' 
      { lvmtok lexbuf } (* Ignore comments *)
  | ['\t' '\n' ' '] 
      { lvmtok lexbuf } (* Ignore whitespace *)
  | digit+ as inum 
      { Integer (Int64.of_string inum) }
  | '\"' [^ '\"']* '\"' as str 
      { String (String.sub str 1 (String.length str - 2)) }
  | '{' { BeginStruct } 
  | '}' { EndStruct } 
  | '[' { BeginArray }
  | ']' { EndArray }
  | '=' { Equals }
  | ',' { Comma }
  | name as str 
      { Ident str }
  | _ as c
      { Printf.printf "Unrecognized character: '%c'\n" c;
	lvmtok lexbuf
      }
  | eof { Eof } 
