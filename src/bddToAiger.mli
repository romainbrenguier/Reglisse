(** Verilog-like declarations *)
type declaration =
| Input of string
| Output of string
| Reg of string
| Wire of string
| DList of declaration list


val add_declaration : declaration AigerBdd.SymbolMap.t -> declaration -> declaration AigerBdd.SymbolMap.t
val of_declaration : declaration list -> declaration AigerBdd.SymbolMap.t


(** arguments are the name and the size of the variable *)
val input : string -> int -> declaration * Integer.t
val output : string -> int -> declaration * Integer.t
val reg : string -> int -> declaration * Integer.t
val wire : string -> int -> declaration * Integer.t

val simple_input : string -> declaration * Boolean.t
val simple_output : string -> declaration * Boolean.t
val simple_reg : string -> declaration * Boolean.t
val simple_wire : string -> declaration * Boolean.t

(*
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
*)

(** The first component gives the expressions that are non synthesizable and raised the exception, the second one gives an expression describing the non synthesizable inputs. *)
exception NonSynthesizable of (Integer.t * Integer.t)

(** Synthesize from a constraint given as a Boolean expression. *)
(*
val synthesize : declaration list -> bool Integer.t -> AigerImperative.t

val add_synthesized : declaration list -> bool Integer.t -> AigerImperative.t -> AigerImperative.t
*)

(** Iterative process, that synthesize a circuit for the first constraint, then if possible add the other constraints. *)
(*
val constraint_synthesis : declaration list -> bool Integer.t list -> AigerImperative.t
*)

(** Synthesize from the definition of how each variable should be updated.
    The expression on the right should be an output or a register.
    Warning: if the expression on the right has size greater than the variable on the left, then its result is truncated. 
*)
val functional_synthesis : (Integer.t * Integer.t) list -> AigerImperative.t

(** list to keep track of currently declared variables *)
val _declarations : declaration list

val print_aiger : AigerImperative.t -> unit
