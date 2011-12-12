{
  open Pcre
  open Simpleconfig_parser
}
let graph = [^ ' ' '\t' '\n' '\'' '"' '=' '[' ']' ',']
let white = ('#' [^ '\n']* '\n' | [ ' ' '\t' '\n' ]+)
rule token = parse
    (* special symbols *)
  | "jid"                                             { JID }
  | "password"                                        { PASSWORD }
  | "resource"                                        { RESOURCE }
  | "permit"                                          { PERMIT }
  | "server"                                          { SERVER }
  | "port"                                            { PORT }
  | "logfile"                                         { LOGFILE }
  | "photo"                                           { PHOTO }
  | "todofile"                                        { TODOFILE }
  | "donefile"                                        { DONEFILE }
    (* syntax tokens *)
  | '='                                               { EQUALS }
  | '['                                               { LBRACKET }
  | ']'                                               { RBRACKET }
  | ','                                               { COMMA }
    (* words, possibly quoted *)
  | graph+ as w                                       { WORD w }
  | '\'' (('\\' '\'' | [^ '\''])* as w) '\''          { let t = qreplace ~pat:"\\\\'" ~templ:"'" w in WORD t }
  | '"'  (('\\' '"' | [^ '"'])* as w) '"'             { let t = qreplace ~pat:"\\\\\"" ~templ:"\"" w in WORD t }
    (* skip whitespace and comments *)
  | white+                                            { token lexbuf }
  | eof                                               { EOF }
