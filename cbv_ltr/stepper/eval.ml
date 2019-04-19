open Syntax
open Context

(* call-by-value かつ left-to-right で式を評価して１簡約ごとに式を出力する *)
let rec f (exp : Syntax.t) (ctxt : Context.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (e1, x, e2) ->
    let (s1, v1) = f e1 (CLet (x, e2) :: ctxt) in
    let new_e2 = subst e2 x v1 in
    memo (Let (v1, x, e2)) new_e2 ctxt "";
    let (s2, v2) = f new_e2 ctxt in
    (s1 ^ s2, v2)
  | True -> ("", True)
  | False -> ("", False)
  | If (e1, e2, e3) ->
    let (s1, v1) = f e1 (CIf (e2, e3) :: ctxt) in
    let (s23, v23) =
      match v1 with
      | True ->
        memo (If (v1, e2, e3)) e2 ctxt "";
        f e2 ctxt
      | False ->
        memo (If (v1, e2, e3)) e3 ctxt "";
        f e3 ctxt
      | _ -> failwith (to_string v1 ^ " is expected to be bool") in
    (s1 ^ s23, v23)
  | Inl (e) ->
    let (s, v) = f e (CInl :: ctxt) in
    (s, Inl (v))
  | Inr (e) ->
    let (s, v) = f e (CInr :: ctxt) in
    (s, Inr (v))
  | Pm (e, xl, el, xr, er) ->
    let (s, v) = f e (CPm (xl, el, xr, er) :: ctxt) in
    let (slr, vlr) = match v with
      | Inl (l) ->
        let new_el = subst el xl l in
        memo (Pm (v, xl, el, xr, er)) new_el ctxt "";
        f new_el ctxt
      | Inr (r) ->
        let new_er = subst er xr r in
        memo (Pm (v, xl, el, xr, er)) new_er ctxt "";
        f new_er ctxt
      | _ ->  failwith ("Type error: " ^ to_string v
                        ^ " is expected to be 'a + 'b") in
    (s ^ slr, vlr)
  | Lam (x, e) -> ("", Lam (x, e))
  | App (e1, e2) ->
    let (s1, v1) = f e1 (CAppL (e2) :: ctxt) in
    let (s2, v2) = f e2 (CAppR (v1) :: ctxt) in
    let (s, v) =
      match v2 with
      | Lam (x, e) ->
        let new_e = subst e x v1 in
        memo (App (v1, v2)) new_e ctxt "";
        f new_e ctxt
      | _ -> failwith ("Type error: " ^ to_string v2
                       ^ "is expected to be 'a -> 'b") in
    (s1 ^ s2 ^ s, v)
  | Print (s, e) ->
    memo exp e ctxt s;
    let (se, ve) = f e ctxt in
    (s ^ se, ve)
