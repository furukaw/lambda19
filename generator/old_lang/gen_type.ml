open Type

let rec f (depth : int) : Type.t =
  if depth <= 1 then TBool
  else if Random.int 2 = 1
  then TPlus (f (depth - 1), f (depth - 1))
  else TFun (f (depth - 1), f (depth - 1))
