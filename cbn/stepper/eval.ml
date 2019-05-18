open Syntax
open Context

(* Eval.f : call-by-name で式を評価して１簡約ごとに式を出力する *)
let rec f (exp : Syntax.t) (ctxt : Context.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (eM, x, eN) ->
    let new_N = subst eN x eM in
    memo (Let (eM, x, eN)) new_N ctxt "";
    f new_N ctxt
  | True -> ("", True)
  | False -> ("", False)
  | If (eM, eN, eN') ->
    let (m, t_true_false) = f eM (CIf (eN, eN') :: ctxt) in
    let (m', tT) =
      match t_true_false with
      | True ->
        memo (If (t_true_false, eN, eN')) eN ctxt "";
        f eN ctxt
      | False ->
        memo (If (t_true_false, eN, eN')) eN' ctxt "";
        f eN' ctxt
      | _ -> failwith ("Type error: " ^ to_string t_true_false
                       ^ " is expected to be bool") in
    (m ^ m', tT)
  | Inl (eM) ->
    ("", Inl (eM))
  | Inr (eM) ->
    ("", Inr (eM))
  | Pm (eM, x, eN, x', eN') ->
    let (m, inlr) = f eM (CPm (x, eN, x', eN') :: ctxt) in
    let (m', tT) =
      match inlr with
      | Inl (eM') ->
        let new_eN = subst eN x eM' in
        memo (Pm (inlr, x, eN, x', eN')) new_eN ctxt "";
        f new_eN ctxt
      | Inr (eM') ->
        let new_eN' = subst eN' x' eM' in
        memo (Pm (inlr, x, eN, x', eN')) new_eN' ctxt "";
        f new_eN' ctxt
      | _ -> failwith ("Type error: " ^ to_string inlr
                       ^ " is expected to be 'a + 'b") in
    (m ^ m', tT)
  | Lam (x, eM) -> ("", Lam (x, eM))
  | App (eM, eN) ->
    let (m, t_lx_N') = f eN (CAppR (eM) :: ctxt) in
    let (m', tT) =
      match t_lx_N' with
      | Lam (x, eN') ->
        let new_eN' = subst eN' x eM in
        memo (App (eM, t_lx_N')) new_eN' ctxt "";
        f new_eN' ctxt
      | _ -> failwith ("Type error: " ^ to_string t_lx_N'
                       ^ " is expected to be 'a -> 'b") in
    (m ^ m', tT)
  | Print (c, eM) ->
    memo (Print (c, eM)) eM ctxt c;
    let (m, tT) = f eM ctxt in
    (c ^ m, tT)
