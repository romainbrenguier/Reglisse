(** Verilog-like declarations *)
type declaration =
| Input of AigerBdd.symbol 
| Output of AigerBdd.symbol 
| Reg of AigerBdd.symbol
| Wire of AigerBdd.symbol
| DList of declaration list


val add_declaration : declaration AigerBdd.SymbolMap.t -> declaration -> declaration AigerBdd.SymbolMap.t
val of_declaration : declaration list -> declaration AigerBdd.SymbolMap.t


(** arguments are the name and the size of the variable *)
val input : string -> int -> declaration * 'a Integer.t
val output : string -> int -> declaration * 'a Integer.t
val reg : string -> int -> declaration * 'a Integer.t
val wire : string -> int -> declaration * 'a Integer.t


module Constraint : 
sig 
  type t
  val of_bdd : Cudd.bdd -> t
  val to_bdd : t -> Cudd.bdd 
  val of_expr : Boolean.t -> t
  val of_exprs : Boolean.t list -> t
  val of_list : t list -> t
  val for_each : int -> int -> (int -> t) -> t
end


(** The first component gives the expressions that are non synthesizable and raised the exception, the second one gives an expression describing the non synthesizable inputs. *)
exception NonSynthesizable of (bool Integer.t * bool Integer.t)

(** Synthesize from a constraint given as a Boolean expression. *)
val synthesize : declaration list -> bool Integer.t -> Aiger.t

val add_synthesized : declaration list -> bool Integer.t -> Aiger.t -> Aiger.t

(** Iterative process, that synthesize a circuit for the first constraint, then if possible add the other constraints. *)
val constraint_synthesis : declaration list -> bool Integer.t list -> Aiger.t

(** Synthesize from the definition of how each variable should be updated.
    The expression on the right should be an output or a register.
    Warning: if the expression on the right has size greater than the variable on the left, then its result is truncated. 
*)
val functional_synthesis : ('a Integer.t * 'a Integer.t) list -> Aiger.t

(** list to keep track of currently declared variables *)
val _declarations : declaration list

val print_aiger : Aiger.t -> unit
