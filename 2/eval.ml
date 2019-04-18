open Syntax

(* Call-By-Value で式を評価する *)
let rec f (exp : Syntax.t) : string * Syntax.t = match exp with
  | Var (x) -> failwith ("Unbound variable: " ^ x)
  | Let (e1, x, e2) ->
    let (s1, v1) = f e1 in
    let (s2, v2) = f (subst e2 x v1) in
    (s1 ^ s2, v2)
  | True -> ("", True)
  | False -> ("", False)
  | If (e1, e2, e3) ->
    let (s1, v1) = f e1 in
    let (s23, v23) =
      match v1 with
      | True -> f e2
      | False -> f e3
      | _ -> failwith (to_string v1 ^ " is expected to be bool") in
    (s1 ^ s23, v23)
  | Inl (e) -> let (s, v) = f e in (s, Inl (v))
  | Inr (e) -> let (s, v) = f e in (s, Inr (v))
  | Pm (e, xl, el, xr, er) ->
    let (s, v) = f e in
    let (slr, vlr) = match v with
      | Inl (l) -> f (subst el xl l)
      | Inr (r) -> f (subst er xr r)
      | _ ->  failwith ("Type error: " ^ to_string v
                        ^ " is expected to be 'a + 'b") in
    (s ^ slr, vlr)
  | Lam (x, e) -> ("", Lam (x, e))
  | App (e1, e2) ->
    let (s1, v1) = f e1 in
    let (s2, v2) = f e2 in
    let (s, v) =
      match v2 with
      | Lam (x, e) -> f (subst e x v1)
      | _ -> failwith ("Type error: " ^ to_string v2
                       ^ "is expected to be 'a -> 'b") in
    (s1 ^ s2 ^ s, v)
  | Print (s, e) ->
    let (se, ve) = f e in
    (s ^ se, ve)
