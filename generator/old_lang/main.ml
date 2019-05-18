(* メイン関数 *)
let go () =
  Random.self_init ();
  let typ =
    if Array.length Sys.argv >= 3
    then Gen_type.f (int_of_string Sys.argv.(2))
    else
      Input.convert (Parser.expr Lexer.token (Lexing.from_channel stdin)) in
  let depth =
    if Array.length Sys.argv >= 2
    then int_of_string Sys.argv.(1)
    else 3 in
  (* 引数がないなら（-no-message が指定されていないなら）*)
  print_string "Type : ";
  Type.print typ;		(* 入力を表示する *)
  print_newline ();
  print_string "Generated : ";
  let result = Gen.f typ Env.empty depth in
  Syntax.print result;
  print_newline ()

(* スタートアップ *)
let _ = go ()
    
