open Syntax
open Context

(* Eval.f : call-by-name で受け取った式を評価する *)
let rec f (exp : Syntax.t) (ctxt : Context.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (eM, x, eN) ->
    let new_N = subst eN x eM in
    memo (Let (eM, x, eN)) new_N ctxt "";
    f new_N ctxt
  | True -> ("", True)
  | False -> ("", False)
  | If (e1, e2, e3) ->
    let (m, tf) = f e1 (CIf (e2, e3) :: ctxt) in
    let (m', t) =
      match tf with
      | True ->
        memo (If (tf, e2, e3)) e2 ctxt "";
        f e2 ctxt
      | False ->
        memo (If (tf, e2, e3)) e3 ctxt "";
        f e3 ctxt
      | _ -> failwith ("Type error: " ^ to_string tf
                       ^ " is expected to be bool") in
    (m ^ m', t)
  | Inl (eM) ->
    ("", Inl (eM))
  | Inr (eM) ->
    ("", Inr (eM))
  | Pm (eM, xl, eN, xr, eN') ->
    let (m, inlr) = f eM (CPm (xl, eN, xr, eN') :: ctxt) in
    let (m', eT) = match inlr with
      | Inl (eM') ->
        let new_N = subst eN xl eM' in
        memo (Pm (inlr, xl, eN, xr, eN')) new_N ctxt "";
        f new_N ctxt
      | Inr (eM') ->
        let new_N' = subst eN' xr eM' in
        memo (Pm (inlr, xl, eN, xr, eN')) new_N' ctxt "";
        f new_N' ctxt
      | _ -> failwith ("Type error: " ^ to_string inlr
                       ^ " is expected to be 'a + 'b") in
    (m ^ m', eT)
  | Lam (x, e) -> ("", Lam (x, e))
  | App (eM, eN) ->
    let (m, lx_N') = f eN (CAppR (eM) :: ctxt) in
    let (m', eT) =
      match lx_N' with
      | Lam (x, eN') ->
        let new_N' = subst eN' x eM in
        memo (App (eM, lx_N')) new_N' ctxt "";
        f new_N' ctxt
      | _ -> failwith ("Type error: " ^ to_string lx_N'
                       ^ " is expected to be 'a -> 'b") in
    (m ^ m', eT)
  | Print (c, eM) ->
    memo (Print (c, eM)) eM ctxt c;
    let (m, eT) = f eM ctxt in
    (c ^ m, eT)
