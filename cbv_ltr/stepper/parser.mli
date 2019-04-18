type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | QQ
  | TRUE
  | FALSE
  | LET
  | IF
  | THEN
  | ELSE
  | LAM
  | APP
  | DOT
  | ARROW
  | BAR
  | COMMA
  | BE
  | PM
  | AS
  | PRINT
  | INL
  | INR
  | VAR of (string)
  | NUM of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
