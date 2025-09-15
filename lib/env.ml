open Names

type 'a env = (name * 'a) list

let empty : 'a env = []

exception NotFound of name

let rec find = function
  | name, [] -> raise @@ NotFound name
  | name, (x, v) :: tail -> if name = x then v else find (name, tail)

let bind (name, v, env) = (name, v) :: env
