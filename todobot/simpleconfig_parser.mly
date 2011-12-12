%{
  let jid = ref ""
  let password = ref ""
  let resource = ref ""
  let photo = ref None
  let permit = ref []
  let logfile = ref None
  let server = ref None
  let port = ref None
  let todofile = ref None
  let donefile = ref None
%}

%token JID PASSWORD RESOURCE PHOTO PERMIT LOGFILE SERVER PORT TODOFILE DONEFILE
%token <string> WORD
%token COMMA
%token LBRACKET RBRACKET
%token EQUALS
%token EOF
%left COMMA
%start main
%type <string * string * string * (string * string) option * string list * string option * string option * int option * string option * string option> main

%%

main:
  | statements EOF                        { $1; (!jid, !password, !resource, !photo, !permit, !logfile, !server, !port, !todofile, !donefile) }
;
statements:
  | statement statements                  { $1; $2 }
  | statement                             { $1 }
;
statement:
  | JID EQUALS WORD                       { jid := $3 }
  | PASSWORD EQUALS WORD                  { password := $3 }
  | RESOURCE EQUALS WORD                  { resource := $3 }
  | PHOTO EQUALS LBRACKET WORD COMMA WORD RBRACKET
                                          { photo := Some ($4, $6) }
  | PERMIT EQUALS WORD                    { permit := [$3] }
  | PERMIT EQUALS LBRACKET words RBRACKET { permit := $4 }
  | LOGFILE EQUALS WORD                   { logfile := Some $3 }
  | SERVER EQUALS WORD                    { server := Some $3 }
  | PORT EQUALS WORD                      { port := Some (int_of_string $3) }
  | TODOFILE EQUALS WORD                  { todofile := Some $3 }
  | DONEFILE EQUALS WORD                  { donefile := Some $3 }
;
words:
  | WORD COMMA words                      { $1 :: $3 }
  | WORD                                  { [$1] }
;
