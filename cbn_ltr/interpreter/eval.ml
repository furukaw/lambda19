open Syntax

(* Eval.f : call-by-name で式を評価する *)
let rec f (exp : Syntax.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (eM, x, eN) ->
    let new_N = subst eN x eM in
    f new_N
  | True -> ("", True)
  | False -> ("", False)
  | If (eM, eN, eN') ->
    let (m, t_true_false) = f eM in
    let (m', tT) =
      match t_true_false with
      | True ->
        f eN
      | False ->
        f eN'
      | _ -> failwith ("Type error: " ^ to_string t_true_false
                       ^ " is expected to be bool") in
    (m ^ m', tT)
  | Inl (eM) ->
    ("", Inl (eM))
  | Inr (eM) ->
    ("", Inr (eM))
  | Pm (eM, x, eN, x', eN') ->
    let (m, inlr) = f eM in
    let (m', tT) =
      match inlr with
      | Inl (eM') ->
        let new_eN = subst eN x eM' in
        f new_eN
      | Inr (eM') ->
        let new_eN' = subst eN' x' eM' in
        f new_eN'
      | _ -> failwith ("Type error: " ^ to_string inlr
                       ^ " is expected to be 'a + 'b") in
    (m ^ m', tT)
  | Lam (x, eM) -> ("", Lam (x, eM))
  | App (eM, eN) ->
    let (m, t_lx_N') = f eN in
    let (m', tT) =
      match t_lx_N' with
      | Lam (x, eN') ->
        let new_eN' = subst eN' x eM in
        f new_eN'
      | _ -> failwith ("Type error: " ^ to_string t_lx_N'
                       ^ " is expected to be 'a -> 'b") in
    (m ^ m', tT)
  | Print (c, eM) ->
    let (m, tT) = f eM in
    (c ^ m, tT)
