(* メイン関数 *)
let go () =
  let program = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  (* これで標準入力を字句解析して、構文解析した結果を program に入れ *)
  if Array.length Sys.argv = 1 then begin
    (* 引数がないなら（-no-message が指定されていないなら）*)
    print_string "Parsed : ";
    Syntax.print program;		(* 入力を表示する *)
    print_newline ();
    let ty = Typing.f program in	(* 型チェックをして *)
    print_string "Type : ";
    Type.print ty;			(* 型を表示する *)
    print_newline ();
    print_endline "Steps : "
  end;
  let (s, v) = Eval.f program [] in
  print_string "Output : \"";
  print_string s;
  print_endline "\"";
  print_string "Result : ";
  print_endline (Syntax.to_string v)

(* スタートアップ *)
let _ = go ()
    
