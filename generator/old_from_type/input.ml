open Type

type t = IBool
       | IPlus of t * t
       | IFun of t * t

let rec convert (input : t) : Type.t = match input with
  | IBool -> TBool
  | IPlus (i1, i2) -> TPlus (convert i1, convert i2)
  | IFun (i1, i2) -> TFun (convert i1, convert i2)
