%{

 open Absty

 let rec fieldstr f =
   match f with 
     | AInt x -> Printf.sprintf "%Ld" x
     | AStr x -> Printf.sprintf "'%s'" x
     | AStruct x -> "{\n" ^ (String.concat "," (List.map (fun (x,y) -> Printf.sprintf "%s: %s\n" x (fieldstr y)) x)) ^ "}\n"
     | AArr x -> "[" ^ (String.concat "," (List.map fieldstr x)) ^ "]"

%}

%token <int64> Integer
%token <string> String
%token BeginStruct EndStruct BeginArray EndArray Comma Equals Eof
%token <string> Ident

%start start

%type <Absty.absty> start

%%

start:  fields { AStruct $1 }
;

fields: 
| Ident BeginStruct fields EndStruct fields { ($1,AStruct $3)::$5 }
| Ident Equals Integer fields { ($1,AInt $3)::$4 }
| Ident Equals String fields { ($1,AStr $3)::$4 }
| Ident Equals BeginArray array EndArray fields { ($1,AArr $4)::$6 }
| Eof { [] }
| /* nothing */ { [] }

array:
| String morearray { (AStr $1)::$2 }
| Integer morearray { (AInt $1)::$2 }
| { [] }

morearray:
| Comma String morearray { (AStr $2)::$3 }
| Comma Integer morearray { (AInt $2)::$3 }
| { [] }


