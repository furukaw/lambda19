(* メイン関数 *)
let go () =
  let program =
    Input.convert (Parser.expr Lexer.token (Lexing.from_channel stdin)) in
  (* これで標準入力を字句解析して、構文解析した結果を program に入れ *)
  let depth =
    if Array.length Sys.argv >= 2
    then int_of_string Sys.argv.(1)
    else 3 in
  (* 引数がないなら（-no-message が指定されていないなら）*)
  print_string "Parsed : ";
  Type.print program;		(* 入力を表示する *)
  print_newline ();
  print_string "Generated : ";
  Random.self_init ();
  let result = Gen.f program Env.empty depth in
  Syntax.print result;
  print_newline ()

(* スタートアップ *)
let _ = go ()
    
