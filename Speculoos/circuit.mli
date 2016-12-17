type t

module VariableSet : Set.S with type elt = BddVariable.t

val of_aiger : AigerImperative.t -> t
  
    (** Updates of the latches are stocked as BDDs. 
	The returned table contain such a BDD for each latch and each output *)
val updates : t -> (BddVariable.t , Cudd.bdd) Hashtbl.t
val variables : t -> VariableSet.t
val next_variables : t -> VariableSet.t
val array_variables : t -> BddVariable.t array
val array_next_variables : t -> BddVariable.t array
val composition_vector : t -> Cudd.bdd array
val map : t -> AigerImperative.lit AigerImpBdd.VariableMap.t
  
val print_valuation : AigerImperative.t -> string list -> bool AigerImpBdd.VariableMap.t -> unit
  
val initial_state : AigerImperative.t -> bool AigerImpBdd.VariableMap.t
