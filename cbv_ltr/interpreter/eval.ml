open Syntax

(* call-by-value かつ left-to-right で式を評価する *)
let rec f (exp : Syntax.t) : string * Syntax.t =
  match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (e1, x, e2) ->
    let (s1, v1) = f e1 in
    let new_e2 = subst e2 x v1 in
    let (s2, v2) = f new_e2 in
    (s1 ^ s2, v2)
  | True -> ("", True)
  | False -> ("", False)
  | If (e1, e2, e3) ->
    let (s1, v1) = f e1 in
    let (s23, v23) =
      match v1 with
      | True ->
        f e2
      | False ->
        f e3
      | _ -> failwith (to_string v1 ^ " is expected to be bool") in
    (s1 ^ s23, v23)
  | Inl (e) ->
    let (s, v) = f e in
    (s, Inl (v))
  | Inr (e) ->
    let (s, v) = f e in
    (s, Inr (v))
  | Pm (e, xl, el, xr, er) ->
    let (s, v) = f e in
    let (slr, vlr) = match v with
      | Inl (l) ->
        let new_el = subst el xl l in
        f new_el
      | Inr (r) ->
        let new_er = subst er xr r in
        f new_er
      | _ ->  failwith ("Type error: " ^ to_string v
                        ^ " is expected to be 'a + 'b") in
    (s ^ slr, vlr)
  | Lam (x, e) -> ("", Lam (x, e))
  | App (e1, e2) ->
    let (s1, v1) = f e1 in
    let (s2, v2) = f e2 in
    let (s, v) =
      match v2 with
      | Lam (x, e) ->
        let new_e = subst e x v1 in
        f new_e
      | _ -> failwith ("Type error: " ^ to_string v2
                       ^ "is expected to be 'a -> 'b") in
    (s1 ^ s2 ^ s, v)
  | Print (s, e) ->
    let (se, ve) = f e in
    (s ^ se, ve)
