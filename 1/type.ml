(* Type.t : プログラムの型を表す型 *)
type t = TBool
       | TPlus of t * t
       | TFun of t * t
       | TVar of t option ref

(* Type.typed_t : 型を持つプログラムの型 *)
type typed_t =
  | Var of string
  (* let x = M in N *) (* M` λx.N *)
  | Let of pair_t * string * pair_t  (* let M be x. N *)
  | True
  | False
  | If of pair_t * pair_t * pair_t
  | Inl of pair_t
  | Inr of pair_t
  (* match M with inl x -> N | inr x -> N' *)
  (* pm M as {inl x.N, inr x.N'} *)
  | Pm of pair_t * string * pair_t * string * pair_t
  | Lam of string * pair_t
  (* 引数 関数 *)
  | App of pair_t * pair_t  (* M` N *)
  (* | print c. M *)  

(* Type.pair_t : 型とプログラムのペアの型 *)
and pair_t = t * typed_t

(* 新しい型変数を作る *)
(* Type.gen_type : unit -> Type.t *)
let gen_type () = TVar (ref None)

(* 型変数を中身で置き換えた型を返す。返ってくる型には型変数は含まれない *)
(* deref_type : Type.t -> Type.t *)
let rec deref_type ty = match ty with
  | TBool -> TBool
  | TPlus (ty1, ty2) ->
    TPlus (deref_type ty1, deref_type ty2)
  | TFun (ty1, ty2) ->
    TFun (deref_type ty1, deref_type ty2)
  | TVar (r) ->
      begin match !r with
	  None ->
	    r := Some (TBool); (* 何でも良い *)
	    TBool
	| Some (ty') ->
	    let ty'' = deref_type ty' in
	    r := Some (ty'');
	    ty''
      end

(* プログラムの型を文字列にする関数 *)
(* Type.to_string : Type.t -> string *)
let rec to_string ty =
  let pq s = "(" ^ s ^ ")" in
  match ty with
  | TBool -> "bool"
  | TPlus (ty1, ty2) -> pq (to_string ty1 ^ " + " ^ to_string ty2)
  | TFun (ty1, ty2) -> pq (to_string ty1 ^ " -> " ^ to_string ty2)
  | TVar (r) -> "tvar"

(* プログラムの型をプリントする関数 *)
(* Type.print : Type.t -> unit *)
let print ty =
  let str = to_string (deref_type ty) in
  print_string str
