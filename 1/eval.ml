open Syntax

(* プログラムを評価する *)
(* Eval.f : Syntax.t -> (string, Value.t) Env.t -> Value.t *)
let rec f exp env = match exp with
  | Var (x) ->
    begin
      try Env.get env x
      with Not_found -> failwith ("Unbound variable: " ^ x)
    end
  | Let (e1, x, e2) ->
    let v1 = f e1 env in
    f e2 (Env.extend env x v1)
  | True -> Value.VTrue
  | False -> Value.VFalse
  | If (e1, e2, e3) ->
    begin
      let v1 = f e1 env in
      match v1 with
      | Value.VTrue -> f e2 env
      | Value.VFalse -> f e3 env
      | _ -> failwith (Value.to_string v1 ^ " is expected to be bool")
    end
  | Inl (e) -> Value.VInl (f e env)
  | Inr (e) -> Value.VInr (f e env)
  | Pm (e, xl, el, xr, er) ->
    begin
      let v = f e env in
      match v with
      | Value.VInl (l) -> f el (Env.extend env xl v)
      | Value.VInr (r) -> f er (Env.extend env xr v)
      | _ -> failwith ("Type error: " ^ Value.to_string v
                       ^ " is expected to be 'a + 'b")
    end
  | Lam (x, e) -> Value.VLam (x, e)
  | App (e1, e2) ->
    begin
      let v1 = f e1 env in
      let v2 = f e2 env in
      match v2 with
      | Value.VLam (x, e) -> f e (Env.extend env x v1)
      | _ -> failwith ("Type error: " ^ Value.to_string v2
                       ^ "is expected to be 'a -> 'b")
    end
