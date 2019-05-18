type ('a, 'b) t
	(* キーが 'a 型、値が 'b 型の環境を表す型 *)

val empty : ('a, 'b) t
	(* 使い方：empty *)
	(* 空の環境 *)

val get : ('a, 'b) t -> 'a -> 'b
	(* 使い方：get env var *)
	(* 環境 env の中で変数 var の値を返す *)
(* 変数 var が見つからなかったら例外 Not_found を起こす *)

val find_all : ('a, 'b) t -> ('a -> bool) -> 'b list
(* 使い方:find_all env var *)
(* 環境 env の中で変数 var の値を全て返す *)

val filter : ('a, 'b) t -> ('a -> bool) -> ('a, 'b) t
(* 使い方:filter env f *)
(* 環境 env の中で f を満たすもののみの環境を返す *)

val nth : ('a, 'b) t -> int -> ('a * 'b)
(* 使い方:nth env n *)
(* 環境 env の n 番目のペアを返す *)

val length : ('a, 'b) t -> int
(* 使い方:length env *)
(* 環境 env の長さを返す *)

val extend : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
	(* 使い方：extend env var value *)
(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
    
