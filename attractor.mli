(** Parse an aiger file and return two lists, one containing the controllables inputs and the other the uncontrollable inputs. *)
val read_from_file : string -> (Aiger.t * Aiger.lit list * Aiger.lit list)
val of_aiger : Aiger.t -> (Aiger.t * Aiger.lit list * Aiger.lit list)

val controllable_name : string -> bool
(** List of controllables and uncontrollables inputs in the AIGER file *)
val controllables : Aiger.t -> (Aiger.lit list * Aiger.lit list)
val controllable_variables : Aiger.t -> (AigerBdd.variable list * AigerBdd.variable list)


(** [attractor aiger controllables uncontrollables goal] 
    Compute the set of states from which the controller can force going to the goal states. By default weak is set to false which means that controller is player 2 
*)
val attractor : AigerBdd.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Cudd.bdd -> Region.t

(** [trap aiger controllables uncontrollables failure] 
    Compute the set of states from which the controller cannot avoid going to the failure states. If the weak flag is set to true, this means that the controller cannot see the uncontrollable actions before taking its decision (ie controller = player 2).
*)
val trap : AigerBdd.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Cudd.bdd -> Region.t

(** Compute the same operator as the previous function, except that the actions are restricted by the last argument *)
val attractor_with_restriction : AigerBdd.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Region.t -> Region.t -> Region.t

(** Compute the same operator as the previous function, except that the first BDD gives restriction on the actions of the controller and the environment *)
val trap_with_restriction : AigerBdd.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Region.t -> Region.t -> Region.t

(** Construct a BDD representing a winning strategy *)
val strategy : AigerBdd.t -> Region.t -> Cudd.bdd

(** Takes the original aiger file, a BDD representing the winning strategy and the controllable and uncontrollable inputs *)
val strategy_to_aiger : Aiger.t -> Cudd.bdd -> AigerBdd.variable list -> AigerBdd.variable list -> Aiger.t

(** Given an aiger file, check that there is a safe strategy to avoid having one output to true. *)
val test : Aiger.t -> unit
