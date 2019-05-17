type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec get env var = match env with
  | [] -> raise Not_found
  | (a, b) :: rest ->
    if a = var then b else get rest var

let rec find_all env f = match env with
  | [] -> []
  | (a, b) :: rest ->
    if f a then b :: find_all rest f else find_all rest f

let rec filter env f = match env with
  | [] -> []
  | (a, b) as first :: rest ->
    if f a then first :: filter rest f else filter rest f

let nth env n = List.nth env n

let length = List.length

let rec extend env var value = (var, value) :: env

