(* Syntax.t : プログラムの型 *)
type t =
  | Var of string
  (* let x = M in N *) (* M` λx.N *)
  | Let of t * string * t  (* let M be x. N *)
  | True
  | False
  | If of t * t * t
  | Inl of t
  | Inr of t
  (* match M with inl x -> N | inr x -> N' *)
  (* pm M as {inl x.N, inr x.N'} *)
  | Pm of t * string * t * string * t
  | Lam of string * t
  (* 引数 関数 *)
  | App of t * t  (* M` N *)
  | Print of string * t  (* print c. M *)

(* プログラムが値かどうかを返す *)
(* Syntax.is_value : Syntax.t -> bool *)
let rec is_value exp = match exp with
  | Var (_) | Let (_, _, _) | If (_, _, _)
  | Pm (_, _, _, _, _) | App (_, _) | Print (_, _) -> false
  | True | False | Lam (_, _) -> true
  | Inl (e) -> is_value e
  | Inr (e) -> is_value e

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string (exp : t) : string =
  let pq s = "(" ^ s ^ ")" in
  match exp with
  | Var (x) -> x
  | Let (e1, x, e2) ->
    pq ("let " ^ to_string e1 ^ " be " ^ x ^ "." ^ to_string e2)
  | True -> "true"
  | False -> "false"
  | If (e1, e2, e3) ->
    pq ("if " ^ to_string e1
        ^ " then " ^ to_string e2
        ^ " else " ^ to_string e3)
  | Inl (e) -> pq ("inl " ^ to_string e)
  | Inr (e) -> pq ("inr " ^ to_string e)
  | Pm (e, xl, el, xr, er) ->
    pq ("pm " ^ to_string e ^ " as {inl " ^ xl ^ "." ^ to_string el ^ ", inr " ^ xr ^ "." ^ to_string er ^ "}")
  | Lam (x, e) -> pq ("λ" ^ x ^ "." ^ to_string e)
  | App (e1, e2) -> pq (to_string e1 ^ "` " ^ to_string e2)
  | Print (s, e) -> pq ("print \"" ^ s ^ "\"." ^ to_string e)

(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_endline str

(* 式 exp の中の変数 x を式 v に置換した式を返す *)
(* Syntax.subst : Syntax.t -> string -> Syntax.t -> Syntax.t *)
let rec subst exp x v = match exp with
  | Var (y) when x = y -> v
  | Let (e1, y, e2) -> Let (subst e1 x v, y, if x = y then e2 else subst e2 x v)
  | If (e1, e2, e3) -> If (subst e1 x v, subst e2 x v, subst e3 x v)
  | Inl (e) -> Inl (subst e x v)
  | Inr (e) -> Inr (subst e x v)
  | Pm (e, xl, el, xr, er) ->
    Pm (subst e x v, xl, (if xl = x then el else subst el x v), xr, (if xr = x then er else subst er x v))
  | Lam (y, e) when x <> y -> Lam (y, subst e x v)
  | App (e1, e2) -> App (subst e1 x v, subst e2 x v)
  | Print (s, e) -> Print (s, subst e x v)
  | _ -> exp
