{
(* 補助的な変数、関数、型などの定義 *)
open Parser
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "bool" { BOOL }
| "->"   { ARROW }
| "+"    { PLUS }
| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }

and strings = parse
| "\""   { ignore (Lexing.lexeme lexbuf); "" }
| eof    { ignore (Format.eprintf "warning: unterminated string@."); "" }
| _      { let s1 = Lexing.lexeme lexbuf in
           let s2 = strings lexbuf in
	   s1 ^ s2 }
