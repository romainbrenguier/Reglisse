(** [attractor aiger controllables uncontrollables goal] 
    Compute the set of states from which the controller can force going to the goal states. By default weak is set to false which means that controller is player 2 
*)
val attractor : Circuit.t -> BddVariable.t list -> BddVariable.t list -> ?weak:bool -> Cudd.bdd -> Region.t

(** [trap aiger controllables uncontrollables failure] 
    Compute the set of states from which the controller cannot avoid going to the failure states.
    If the weak flag is set to true, this means that the controller cannot see the uncontrollable actions before taking its decision (ie controller = player 2).
    The optional strategy gives restriction on the actions of the controller.
*)
val trap : ?weak:bool -> ?strategy:Strategy.t -> Game.t -> Region.t

(** Negation of the trap *)
val safe : ?weak:bool -> ?strategy:Strategy.t -> Game.t -> Region.t

(** Given an aiger file, check that there is a safe strategy to avoid having one output to true. *)
val test : AigerImperative.t -> unit
