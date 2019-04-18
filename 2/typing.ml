open Syntax

(* 型を同じにできない場合に起きる例外 *)
exception Unify of Type.t * Type.t
(* 自由変数がある場合に起きる例外 *)
exception UnboundVariable of string

(* r が型 ty に現れるかをチェックする (occur check) *)
(* occur : Type.t option ref -> Type.t -> bool *)
let rec occur r ty = match ty with
  | Type.TBool -> false
  | Type.TPlus (ty1, ty2) -> occur r ty1 || occur r ty2
  | Type.TFun (ty1, ty2) -> occur r ty1 || occur r ty2
  | Type.TVar(r') ->
      if r == r' then true
      else begin match !r' with
	  None -> false
	| Some (ty') -> occur r ty'
      end

(* ty1 = ty2 となるように、型変数への代入をする *)
(* unify : Type.t -> Type.t -> unit *)
let rec unify ty1 ty2 = match (ty1, ty2) with
  | (Type.TBool, Type.TBool) -> ()
  | (Type.TPlus (ty11, ty12), Type.TPlus (ty21, ty22)) ->
    begin
      unify ty11 ty21;
      unify ty12 ty22
    end
  | (Type.TFun (ty11, ty12), Type.TFun (ty21, ty22)) ->
    begin
      unify ty11 ty21;
      unify ty12 ty22
    end
  | (Type.TVar (r1), Type.TVar (r2)) when r1 == r2 -> ()
  | (Type.TVar ({ contents = Some(ty1') }), _) -> unify ty1' ty2
  | (_, Type.TVar ({ contents = Some(ty2') })) -> unify ty1 ty2'
  | (Type.TVar (r1), _) ->
      begin match !r1 with
	  None -> if occur r1 ty2 then raise (Unify (ty1, ty2))
				  else r1 := Some (ty2)
	| Some (ty1') -> unify ty1' ty2
      end
  | (_, Type.TVar (r2)) ->
      begin match !r2 with
	  None -> if occur r2 ty1 then raise (Unify (ty1, ty2))
				  else r2 := Some (ty1)
	| Some (ty2') -> unify ty1 ty2'
      end
  | (_, _) -> raise (Unify (ty1, ty2))

(* 型推論 *)
(* g : Syntax.t -> (string, Type.t) Env.t -> Type.t *)
let rec g expr tenv =
  try
    begin match expr with
      | Var (x) -> Env.get tenv x
      | Let (e1, x, e2) ->
        let ty1 = g e1 tenv in
        g e2 (Env.extend tenv x ty1)
      | True -> Type.TBool
      | False -> Type.TBool
      | If (e1, e2, e3) ->
        let ty1 = g e1 tenv in
        unify ty1 Type.TBool;
        let ty2 = g e2 tenv in
        let ty3 = g e3 tenv in
        unify ty2 ty3;
        ty2
      | Inl (e) ->
        let ty = g e tenv in
        Type.TPlus (ty, Type.gen_type ())
      | Inr (e) ->
        let ty = g e tenv in
        Type.TPlus (Type.gen_type (), ty)
      | Pm (e1, xl, el, xr, er) ->
        begin
          let ty1 = g e1 tenv in
          match ty1 with
          | Type.TPlus(tyl, tyr) ->
            let ty2 = g el (Env.extend tenv xl tyl) in
            let ty3 = g er (Env.extend tenv xr tyr) in
            unify ty2 ty3;
            ty2
          | _ -> raise
                   (Unify (ty1, Type.TPlus(Type.gen_type (), Type.gen_type ())))
        end
      | Lam (x, e) ->
        let ty = Type.gen_type () in
        Type.TFun(ty, g e (Env.extend tenv x ty))
      | App (e1, e2) ->
        let ty1 = g e1 tenv in
        let ty2 = g e2 tenv in
        let ty = Type.gen_type () in
        unify ty2 (Type.TFun (ty1, ty));
        ty
      | Print (_, e) ->
        g e tenv
    end
  with Unify (ty1, ty2) -> begin (* unify できなかった *)
    print_endline "式";
    print_string "	";
    Syntax.print expr;
    print_newline ();
    print_endline "を型推論中に型エラーがおきました。";
    print_string "	";
    Type.print ty1;
    print_newline ();
    print_endline "と";
    print_string "	";
    Type.print ty2;
    print_newline ();
    print_endline "は unify できません。";
    exit 0
  end
     | UnboundVariable (x) -> begin
         print_endline "式";
         print_string "	";
         Syntax.print expr;
         print_newline ();
         print_endline "を型推論中に未定義エラーがおきました。";
         print_string "	";
         print_endline x;
         print_endline "は定義されていません。";
         exit 0
       end

(* 型推論の入り口 *)
(* Typing.f : Syntax.t -> Type.t *)
let f expr =
  let ty = g expr Env.empty in
  ty
