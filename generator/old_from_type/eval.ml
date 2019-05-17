open Syntax

(* call-by-value かつ left-to-right で式を評価する *)
let rec f (exp : Syntax.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (eM, x, eN) ->
    let (m, vV) = f eM in
    let new_eN = subst eN x vV in
    let (m', vW) = f new_eN in
    (m ^ m', vW)
  | True -> ("", True)
  | False -> ("", False)
  | If (eM, eN, eN') ->
    let (m, v_true_false) = f eM in
    let (m', vV) =
      match v_true_false with
      | True ->
        f eN
      | False ->
        f eN'
      | _ -> failwith ("Type error: " ^ to_string v_true_false
                       ^ " is expected to be bool") in
    (m ^ m', vV)
  | Inl (eM) ->
    let (m, vV) = f eM in
    (m, Inl (vV))
  | Inr (eM) ->
    let (m, vV) = f eM in
    (m, Inr (vV))
  | Pm (eM, x, eN, x', eN') ->
    let (m, v_inl_inr) = f eM in
    let (m', vW) =
      match v_inl_inr with
      | Inl (vV) ->
        let new_eN = subst eN x vV in
        f new_eN
      | Inr (vV) ->
        let new_eN' = subst eN' x' vV in
        f new_eN'
      | _ -> failwith ("Type error: " ^ to_string v_inl_inr
                        ^ " is expected to be 'a + 'b") in
    (m ^ m', vW)
  | Lam (x, eM) -> ("", Lam (x, eM))
  | App (eM, eN) ->
    let (m, vV) = f eM in
    let (m', v_lx_N') = f eN in
    let (m'', vW) =
      match v_lx_N' with
      | Lam (x, eN') ->
        let new_eN' = subst eN' x vV in
        f new_eN'
      | _ -> failwith ("Type error: " ^ to_string v_lx_N'
                       ^ " is expected to be 'a -> 'b") in
    (m ^ m' ^ m'', vW)
  | Print (c, eM) ->
    let (m, vV) = f eM in
    (c ^ m, vV)
