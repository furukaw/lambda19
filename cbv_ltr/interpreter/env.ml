type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec get env var = match env with
  | [] -> raise Not_found
  | (a, b) :: rest ->
    if a = var then b else get rest var

let rec extend env var value = (var, value) :: env

