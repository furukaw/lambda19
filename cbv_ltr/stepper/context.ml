open Syntax

(* Context.frame_t : コンテキストフレームの型 *)
type frame_t =
  | CLet of string * Syntax.t                     (* let [.] be ... *)
  | CIf of Syntax.t * Syntax.t                    (* if [.] then ... *)
  | CInl                                          (* inl [.] *)
  | CInr                                          (* inr [.] *)
  | CPm of string * Syntax.t * string * Syntax.t  (* pm [.] as ... *)
  | CAppL of Syntax.t                             (* [.] M *)
  | CAppR of Syntax.t                             (* V [.] *)

(* Context.t : コンテキストの型 *)
type t = frame_t list

(* Context.step : ステップ番号 *)
let step = ref 0

(* Context.printed_string : これまでに出力されたことになっている文字列 *)
let printed_string = ref ""

(* 今簡約している部分式とコンテキストを受け取ってプログラム全体を返す *)
let rec plug (exp : Syntax.t) (ctxt : t) : Syntax.t = match ctxt with
  | [] -> exp
  | first :: rest -> match first with
    | CLet (x, e) -> plug (Let (exp, x, e)) rest
    | CIf (e2, e3) -> plug (If (exp, e2, e3)) rest
    | CInl -> plug (Inl (exp)) rest
    | CInr -> plug (Inr (exp)) rest
    | CPm (xl, el, xr, er) -> plug (Pm (exp, xl, el, xr, er)) rest
    | CAppL (er) -> plug (App (exp, er)) rest
    | CAppR (el) -> plug (App (el, exp)) rest

(* 簡約前後の部分式とコンテキストを受け取って簡約前後のプログラムを出力する *)
let memo (redex : Syntax.t ) (reduct : Syntax.t) (ctxt : t)
    (new_output : string) : unit =
  let print_num () = print_string ("  Step " ^ string_of_int !step ^ " : ") in
  let print_output () =
    let spaces = if !step < 10 then " " else "  " in
    print_endline ("  Output" ^ spaces ^ ": " ^ !printed_string) in
  let program_before = plug redex ctxt in
  let program_after = plug reduct ctxt in
  let new_step = !step + 1 in
  print_num ();
  Syntax.print program_before;
  print_output ();
  step := new_step;
  printed_string := !printed_string ^ new_output;
  print_num ();
  Syntax.print program_after;
  print_output ();
  print_newline ()
