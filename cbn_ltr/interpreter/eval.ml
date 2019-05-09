open Syntax

(* Eval.f : call-by-name で受け取った式を評価する *)
let rec f (exp : Syntax.t) : string * Syntax.t = match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (eM, x, eN) ->
    let new_N = subst eN x eM in
    f new_N
  | True -> ("", True)
  | False -> ("", False)
  | If (e1, e2, e3) ->
    let (m, tf) = f e1 in
    let (m', t) =
      match tf with
      | True ->
        f e2
      | False ->
        f e3
      | _ -> failwith ("Type error: " ^ to_string tf
                       ^ " is expected to be bool") in
    (m ^ m', t)
  | Inl (eM) ->
    ("", Inl (eM))
  | Inr (eM) ->
    ("", Inr (eM))
  | Pm (eM, xl, eN, xr, eN') ->
    let (m, inlr) = f eM in
    let (m', eT) =
      match inlr with
      | Inl (eM') ->
        let new_N = subst eN xl eM' in
        f new_N
      | Inr (eM') ->
        let new_N' = subst eN' xr eM' in
        f new_N'
      | _ -> failwith ("Type error: " ^ to_string inlr
                       ^ " is expected to be 'a + 'b") in
    (m ^ m', eT)
  | Lam (x, e) -> ("", Lam (x, e))
  | App (eM, eN) ->
    let (m, lx_N') = f eN in
    let (m', eT) =
      match lx_N' with
      | Lam (x, eN') ->
        let new_N' = subst eN' x eM in
        f new_N'
      | _ -> failwith ("Type error: " ^ to_string lx_N'
                       ^ " is expected to be 'a -> 'b") in
    (m ^ m', eT)
  | Print (c, eM) ->
    let (m, eT) = f eM in
    (c ^ m, eT)
