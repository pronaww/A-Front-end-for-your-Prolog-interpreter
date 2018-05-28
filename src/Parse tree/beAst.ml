type exp = Int of int | Float of float | Symbolic_const of string | String of string | Bool of bool
   | Plus of exp * exp
   | Minus of exp * exp
   | Times of exp * exp
   | Div of exp * exp
   | Mod of exp * exp
   | Expo of exp * exp
   | Equal of exp * exp
   | Gt of exp * exp
   | Lt of exp * exp
   | Ge of exp * exp
   | Le of exp * exp

type term = Var of string | Const of exp | Func of string * (term list)

type atomic_formula = Symbol of string * term list | FAIL | CUT

type goal = Sequence of atomic_formula list

type head = atomic_formula

type body = Subgoals of atomic_formula list

type clause = Fact of head | Rule of head * body

type program = clause list
