type name = string
type value = int

type def =
  | Val of (name * exp)
  | Exp of exp
  | Define of (name * name list * exp)

and exp =
  | Literal of value
  | Var of name
  | Set of (name * exp)
  | If of (exp * exp * exp)
  | While of (exp * exp)
  | Begin of exp list
  | Apply of (name * exp list)
