%token <string> STRING BEGIN
%token END EOF

%start all
%type <Template.t list> all

%%

all: 
| template_list EOF { $1 }
| STRING template_list EOF { $2 }
| template_list STRING EOF { $1 }
| STRING template_list STRING EOF { $2 }

template_list: { [] }
| template template_list { $1 :: $2 };
| template STRING template_list { $1 :: $3 };

template: 
| BEGIN part_list END
    { { Template.name = $1; parts = $2 } };

part_list: { [] }
| part part_list { $1 :: $2 };

part:
| BEGIN  { Template.Hole $1 }
| STRING { Template.String $1 }

