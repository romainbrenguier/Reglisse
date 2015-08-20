type t 

(** Construct a BDD representing a winning strategy *)
val of_region : AigerBdd.Circuit.t -> Region.t -> t

val disj : t list -> t
val conj : t list -> t

(** Takes a BDD representing the winning strategy and the controllable and uncontrollable inputs and returns individual BDDs to control the controllable inputs *)
val to_bdds : Cudd.bdd -> AigerBdd.variable list -> AigerBdd.variable list -> (AigerBdd.variable * Cudd.bdd) list

(** Takes the original aiger file, a BDD representing the winning strategy and the controllable and uncontrollable inputs *)
val to_aiger : Aiger.t -> t -> AigerBdd.variable list -> AigerBdd.variable list -> Aiger.t
