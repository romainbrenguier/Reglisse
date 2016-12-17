(** This module defines usefull function for going from the aiger representation 
 * to the BDD representation and reciproqually *)

(** Initialize CUDD. 
    The number of variables to be used is inferred from the 
    circuit that is provided. *)
val init : AigerImperative.t -> unit

module SymbolMap : Map.S with type key = string
module SymbolSet : Set.S with type elt = string

module VariableMap : Map.S with type key = BddVariable.t

val map_of_aiger : AigerImperative.t -> AigerImperative.lit VariableMap.t
val map_to_string : AigerImperative.lit VariableMap.t -> string

(** Raised by [bdd_to_aiger] if one of the literals in the BDD is not already in the given aiger *)
exception UndeclaredLit of AigerImperative.lit

(** Add to the aiger file, gates to compute the result of the 
    evaluation of the BDD. The result contains the new aiger
    file and the literal that points to the result. 
    All the variables of the BDD should correspond to literals of the given aiger file, or be inside the given VariableMap.
    otherwise a [UndeclaredLit] exception will be raised.
*)
val add_bdd_to_aiger : AigerImperative.t -> AigerImperative.lit VariableMap.t -> Cudd.bdd -> AigerImperative.lit


exception Unsatisfiable of (AigerImperative.t * Cudd.bdd)

(*
(** Takes as argument the list of inputs, list of latches and list of outputs of the circuit to produce *)
val bdd_to_aiger : inputs:symbol list -> latches:symbol list -> outputs:symbol list -> wires:symbol list -> Cudd.bdd -> AigerImperative.t
*)

(** the Bdd tell how to update the different latches *)
val bdds_to_aiger : inputs:string list -> latches:(string * Cudd.bdd) list -> outputs:(string * Cudd.bdd) list -> AigerImperative.t

val valuation_of_list : (BddVariable.t * bool) list -> bool VariableMap.t


(** Convert the valuation to a BDD representation *)
val bdd_of_valuation : bool VariableMap.t -> Cudd.bdd

val bdd_to_valuations : Cudd.bdd -> BddVariable.t list -> (bool VariableMap.t) list


(** Fixpoint of an operation on BDD's *)
val fixpoint : (Cudd.bdd -> Cudd.bdd) -> Cudd.bdd -> Cudd.bdd
							   
(** Reorder the gates of an aiger file so that the index on the left of a gate is always greater than those on the right *)
val reorder_aiger : AigerImperative.t -> AigerImperative.t

