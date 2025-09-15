type name = string
type value = Int of int | Array of value array

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

and func = UserDef of name list * exp | Primitive of (value list -> value)
