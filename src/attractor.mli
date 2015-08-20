(** [attractor aiger controllables uncontrollables goal] 
    Compute the set of states from which the controller can force going to the goal states. By default weak is set to false which means that controller is player 2 
*)
val attractor : AigerBdd.Circuit.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Cudd.bdd -> Region.t

(** [trap aiger controllables uncontrollables failure] 
    Compute the set of states from which the controller cannot avoid going to the failure states. If the weak flag is set to true, this means that the controller cannot see the uncontrollable actions before taking its decision (ie controller = player 2).
*)
val trap : AigerBdd.Circuit.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Cudd.bdd -> Region.t

(** Compute the same operator as the previous function, except that the actions are restricted by the last argument *)
val attractor_with_restriction : AigerBdd.Circuit.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Region.t -> Region.t -> Region.t

(** Compute the same operator as the previous function, except that the first BDD gives restriction on the actions of the controller and the environment *)
val trap_with_restriction : AigerBdd.Circuit.t -> AigerBdd.variable list -> AigerBdd.variable list -> ?weak:bool -> Region.t -> Region.t -> Region.t


(** Given an aiger file, check that there is a safe strategy to avoid having one output to true. *)
val test : Aiger.t -> unit
