(** Parametric modules *)


(** Types for parameters *)
type parameter = 
| Int of int
| String of string 
| Bool of bool

exception WrongParameter of parameter

val get_int : parameter -> int
val get_string : parameter -> string
val get_bool : parameter -> bool


(** Type for parametric module *)
type t = (string -> parameter) -> Aiger.t

type link = 
| True | False | Symb of Aiger.Symbol.t | Not of Aiger.Symbol.t

(** Verilog-like declarations *)
type declaration =
| Wire of Aiger.Symbol.t
| Input of Aiger.Symbol.t 
| Output of Aiger.Symbol.t 
| Reg of Aiger.Symbol.t
| UpdateReg of Aiger.Symbol.t * link
| EqualAnd of Aiger.Symbol.t * link  * link 
| UseModule of Aiger.t * (Aiger.Symbol.t * Aiger.Symbol.t) list
| UseParametricModule of t * (string * parameter) list * (Aiger.Symbol.t * Aiger.Symbol.t) list

val vector_input : string -> int -> int -> declaration list
val vector_output : string -> int -> int -> declaration list
val vector_wire : string -> int -> int -> declaration list
val vector_reg : string -> int -> int -> declaration list

val of_declaration : declaration list -> Aiger.t

val add_declaration : (Aiger.lit Aiger.SymbolMap.t * bool Aiger.SymbolMap.t * Aiger.t) -> declaration -> (Aiger.lit Aiger.SymbolMap.t * bool Aiger.SymbolMap.t *Aiger.t)

type expr = 
| EVar of Aiger.Symbol.t
| ENot of expr
| EAnd of expr * expr
| EOr of expr * expr
| EXor of expr * expr
| ETrue 
| EFalse

val expr_to_declarations : Aiger.Symbol.t -> expr -> declaration list
val expr_to_register_declarations : Aiger.Symbol.t -> expr -> declaration list

