open Syntax
open Type

type t = Type.t -> (Type.t, string) Env.t -> int -> Syntax.t

let p_print = 4

let rec copy (t : Type.t) : Type.t = match t with
  | TBool -> TBool
  | TPlus (t1, t2) -> TPlus (copy t1, copy t2)
  | TFun (t1, t2) -> TFun (copy t1, copy t2)
  | TVar (r) ->
    begin match !r with
      | None -> TVar (ref None)
      | Some (t0) -> TVar (ref (Some (copy t0)))
    end

let unifiable a b =
  try ignore(Typing.unify (copy a) (copy b)); true
  with Typing.Unify (_, _) -> false

let type_size (t : Type.t) (env : (Type.t, string) Env.t) =
  let rec size (t : Type.t) = match t with
    | TBool -> 1
    | TPlus (t1, t2) -> (max (size t1) (size t2)) + 1
    | TFun (t1, t2) -> size t2 + 1
    | TVar (r) ->
      let vars = Env.find_all env (unifiable t) in
      if List.length vars = 0
      then begin match !r with
        | None -> 1
        | Some (t0) -> size t
      end
      else 1
  in size t

let gen_key : int ref = ref 0
let gen_init : unit = gen_key := 0
let gen_env : (int * string) list = [
  (1, "a"); (2, "b"); (3, "c"); (4, "d"); (5, "e");
  (6, "f"); (7, "g"); (8, "h"); (9, "i"); (10, "j");
  (11, "k"); (12, "l"); (13, "m"); (14, "n"); (15, "o");
  (16, "p"); (17, "q"); (18, "r"); (19, "s"); (20, "t");
  (21, "u"); (22, "v"); (23, "w"); (24, "x"); (25, "y"); (26, "z")
]

let gen_var () : string =
  let num = !gen_key + 1 in
  gen_key := num;
  try snd (List.find (fun (i, _) -> i = num) gen_env)
  with Not_found -> failwith "Not supported: too many variables"

let gen_int_key : int ref = ref 0
let gen_num () : string =
  let num = !gen_int_key + 1 in
  gen_int_key := num;
  string_of_int num

(* Gen.f : 型と構文木の深さを受け取って、式を作って返す *)
let rec f : t = fun typ env depth ->
  let vars = Env.filter env (fun t -> t = typ) in
  let vars_num = Env.length vars in
  let limit = type_size typ env in
  let saving = depth <= limit in
  match typ with
  | TBool ->
    if saving
    then
      let ran = Random.int (2 + vars_num) in
      begin match ran with
        | 0 -> True
        | 1 -> False
        | _ ->
          let (var, name) = Env.nth vars (ran - 2) in
          Typing.unify typ var;
          Var (name)
      end
    else
    if Random.int p_print = 0
    then make_print typ env (depth - 1)
    else
      let ran = Random.int 4 in
      begin match ran with
        | 0 -> make_let typ env depth
        | 1 -> make_if typ env depth
        | 2 -> make_pm typ env depth
        | 3 -> make_app typ env depth
        | _ -> failwith "Error(1)"
      end
  | TPlus (t1, t2) ->
    if saving
    then
      let ran = Random.int (2 + vars_num) in
      begin match ran with
        | 0 -> Inl (f t1 env (depth - 1))
        | 1 -> Inr (f t2 env (depth - 1))
        | _ ->
          let (var, name) = Env.nth vars (ran - 2) in
          Typing.unify typ var;
          Var (name)
      end
    else
    if Random.int p_print = 0
    then make_print typ env (depth - 1)
    else
      let ran = Random.int 4 in
      begin match ran with
        | 0 -> make_let typ env depth
        | 1 -> make_if typ env depth
        | 2 -> make_pm typ env depth
        | 3 -> make_app typ env depth
        | _ -> failwith "Error(2)"
      end
  | TFun (t1, t2) ->
    if saving
    then
      let ran = Random.int (1 + vars_num) in
      begin match ran with
        | 0 -> make_lam t1 t2 env depth
        | _ ->
          let (var, name) = Env.nth vars (ran - 1) in
          Typing.unify typ var;
          Var (name)
      end
    else
    if Random.int p_print = 0
    then make_print typ env (depth - 1)
    else
      let ran = Random.int 4 in
      begin match ran with
        | 0 -> make_let typ env depth
        | 1 -> make_if typ env depth
        | 2 -> make_pm typ env depth
        | 3 -> make_app typ env depth
        | _ -> failwith "Error(3)"
      end
  | TVar (r) -> match !r with
    | None ->
      if saving
      then
        begin
          r := Some TBool;
          f TBool env depth
        end
      else
        let ran = Random.int 3 in
        begin match ran with
          | 0 ->
            r := Some TBool;
            f TBool env depth
          | 1 ->
            let (t1, t2) = (Type.gen_type (), Type.gen_type ()) in
            r := Some (TPlus (t1, t2));
            f (TPlus (t1, t2)) env depth
          | 2 ->
            let (t1, t2) = (Type.gen_type (), Type.gen_type ()) in
            r := Some (TFun (t1, t2));
            f (TFun (t1, t2)) env depth
          | _ -> failwith "Error(4)"
        end
    | Some t -> f t env depth

and make_let : t = fun typ env depth ->
  let t1 = Type.gen_type () in
  let e1 = f t1 env (depth - 1) in
  let x = gen_var () in
  let new_env = Env.extend env t1 x in
  let e2 = f typ new_env (depth - 1) in
  Let (e1, x, e2)

and make_if : t = fun typ env depth ->
  let e1 = f TBool env (depth - 1) in
  let e2 = f typ env (depth - 1) in
  let e3 = f typ env (depth - 1) in
  If (e1, e2, e3)

and make_pm : t = fun typ env depth ->
  let (tl, tr) = (Type.gen_type (), Type.gen_type ()) in
  let t1 = Type.TPlus (tl, tr) in
  let e1 = f t1 env (depth - 1) in
  let x = gen_var () in
  let e2 = f typ (Env.extend env tl x) (depth - 1) in
  let e3 = f typ (Env.extend env tr x) (depth - 1) in
  Pm (e1, x, e2, x, e3)

and make_app : t = fun typ env depth ->
  let t1 = Type.gen_type () in
  let e1 = f t1 env (depth - 1) in
  let e2 = f (TFun (t1, typ)) env (depth - 1) in
  App (e1, e2)

and make_lam
    (t1 : Type.t)
    (t2 : Type.t)
    (env : (Type.t, string) Env.t)
    (depth : int)
  : Syntax.t =
  let x = gen_var () in
  let e = f t2 (Env.extend env t1 x) (depth - 1) in
  Lam (x, e)
    
and make_print : t = fun typ env depth ->
  let str = gen_num () in
  let e = f typ env depth in
  Print (str, e)

