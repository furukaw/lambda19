(* Value.t : プログラムの実行結果を表す型 *)
type t = VVar of string
       | VTrue
       | VFalse
       | VInl of t
       | VInr of t
       | VLam of string * Syntax.t
  
(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value =
  let pq s = "(" ^ s ^ ")" in
  match value with
  | VVar (x) -> x
  | VTrue -> "true"
  | VFalse -> "false"
  | VInl (v) -> pq ("inl " ^ to_string v)
  | VInr (v) -> pq ("inr " ^ to_string v)
  | VLam (x, e) -> pq ("λ" ^ x ^ "." ^ Syntax.to_string e)

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
    
