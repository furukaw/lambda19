open Syntax
open Context

(* call-by-value かつ left-to-right で式を評価して１簡約ごとに式を出力する *)
let rec f (exp : Syntax.t) (ctxt : Context.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (eM, x, eN) ->
    let (m, vV) = f eM (CLet (x, eN) :: ctxt) in
    let new_eN = subst eN x vV in
    memo (Let (vV, x, eN)) new_eN ctxt "";
    let (m', vW) = f new_eN ctxt in
    (m ^ m', vW)
  | True -> ("", True)
  | False -> ("", False)
  | If (eM, eN, eN') ->
    let (m, v_true_false) = f eM (CIf (eN, eN') :: ctxt) in
    let (m', vV) =
      match v_true_false with
      | True ->
        memo (If (v_true_false, eN, eN')) eN ctxt "";
        f eN ctxt
      | False ->
        memo (If (v_true_false, eN, eN')) eN' ctxt "";
        f eN' ctxt
      | _ -> failwith ("Type error: " ^ to_string v_true_false
                       ^ " is expected to be bool") in
    (m ^ m', vV)
  | Inl (eM) ->
    let (m, vV) = f eM (CInl :: ctxt) in
    (m, Inl (vV))
  | Inr (eM) ->
    let (m, vV) = f eM (CInr :: ctxt) in
    (m, Inr (vV))
  | Pm (eM, x, eN, x', eN') ->
    let (m, v_inl_inr) = f eM (CPm (x, eN, x', eN') :: ctxt) in
    let (m', vW) =
      match v_inl_inr with
      | Inl (vV) ->
        let new_eN = subst eN x vV in
        memo (Pm (v_inl_inr, x, eN, x', eN')) new_eN ctxt "";
        f new_eN ctxt
      | Inr (vV) ->
        let new_eN' = subst eN' x' vV in
        memo (Pm (v_inl_inr, x, eN, x', eN')) new_eN' ctxt "";
        f new_eN' ctxt
      | _ ->  failwith ("Type error: " ^ to_string v_inl_inr
                        ^ " is expected to be 'a + 'b") in
    (m ^ m', vW)
  | Lam (x, eM) -> ("", Lam (x, eM))
  | App (eM, eN) ->
    let (m, vV) = f eM (CAppL (eN) :: ctxt) in
    let (m', v_lx_N') = f eN (CAppR (vV) :: ctxt) in
    let (m'', vW) =
      match v_lx_N' with
      | Lam (x, eN') ->
        let new_eN' = subst eN' x vV in
        memo (App (vV, v_lx_N')) new_eN' ctxt "";
        f new_eN' ctxt
      | _ -> failwith ("Type error: " ^ to_string v_lx_N'
                       ^ " is expected to be 'a -> 'b") in
    (m ^ m' ^ m'', vW)
  | Print (c, eM) ->
    memo (Print (c, eM)) eM ctxt c;
    let (m, vV) = f eM ctxt in
    (c ^ m, vV)
