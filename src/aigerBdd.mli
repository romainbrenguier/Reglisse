(** This module defines usefull function for going from the aiger representation 
 * to the BDD representation and reciproqually *)

(** Initialize CUDD. 
    The number of variables to be used is inferred from the 
    circuit that is provided. *)
val init : Aiger.t -> unit

type symbol = string * int
val of_aiger_symbol : Aiger.Symbol.t -> symbol
module SymbolMap : Map.S with type key = symbol
module SymbolSet : Set.S with type elt = symbol
val symbol_to_string : symbol -> string

module Variable :
sig
  type t
  val find : symbol -> t
  val to_bdd : t -> Cudd.bdd
  val next : t -> t
  val make_cube : t list -> Cudd.cube
  val to_int : t -> int
  val to_string : t -> string
  val of_lit : Aiger.t -> Aiger.lit -> t 
  val max_var : unit -> int
end

type variable = Variable.t

module VariableMap : Map.S with type key = Variable.t
module VariableSet : Set.S with type elt = Variable.t

val map_of_aiger : Aiger.t -> Aiger.lit VariableMap.t
val map_to_string : Aiger.lit VariableMap.t -> string

(** Raised by [bdd_to_aiger] if one of the literals in the BDD is not already in the given aiger *)
exception UndeclaredLit of Aiger.lit

(** Add to the aiger file, gates to compute the result of the 
    evaluation of the BDD. The result contains the new aiger
    file and the literal that points to the result. 
    All the variables of the BDD should correspond to literals of the given aiger file, or be inside the given VariableMap.
    otherwise a [UndeclaredLit] exception will be raised.
*)
val add_bdd_to_aiger : Aiger.t -> Aiger.lit VariableMap.t -> Cudd.bdd -> (Aiger.t * Aiger.lit)


exception Unsatisfiable of (Aiger.t * Cudd.bdd)

(** Takes as argument the list of inputs, list of latches and list of outputs of the circuit to produce *)
val bdd_to_aiger : inputs:symbol list -> latches:symbol list -> outputs:symbol list -> wires:symbol list -> Cudd.bdd -> Aiger.t

(** the Bdd tell how to update the different latches *)
val bdds_to_aiger : inputs:symbol list -> latches:(symbol * Cudd.bdd) list -> outputs:(symbol * Cudd.bdd) list -> Aiger.t

val valuation_of_list : (Variable.t * bool) list -> bool VariableMap.t


(** Convert the valuation to a BDD representation *)
val bdd_of_valuation : bool VariableMap.t -> Cudd.bdd

val bdd_to_valuations : Cudd.bdd -> Variable.t list -> (bool VariableMap.t) list


(** Fixpoint of an operation on BDD's *)
val fixpoint : (Cudd.bdd -> Cudd.bdd) -> Cudd.bdd -> Cudd.bdd
							   
(** Reorder the gates of an aiger file so that the index on the left of a gate is always greater than those on the right *)
val reorder_aiger : Aiger.t -> Aiger.t

module Circuit :
  sig
    type t
	   
    val of_aiger : Aiger.t -> t
				
    (** Updates of the latches are stocked as BDDs. 
    The returned table contain such a BDD for each latch and each output *)
    val updates : t -> (Variable.t , Cudd.bdd) Hashtbl.t
    val variables : t -> VariableSet.t
    val next_variables : t -> VariableSet.t
    val array_variables : t -> Variable.t array
    val array_next_variables : t -> Variable.t array
    val composition_vector : t -> Cudd.bdd array
    val map : t -> Aiger.lit VariableMap.t
		     
    val rename_configuration : Cudd.bdd -> Variable.t array -> Variable.t array -> Cudd.bdd
										     
    val print_valuation : Aiger.t -> string list -> bool VariableMap.t -> unit
									    
    val initial_state : Aiger.t -> bool VariableMap.t
					
  end
