type t 

(** Construct a BDD representing a winning strategy that enters 
 *  the region when possible and is not defined otherwise *)
val of_region : Circuit.t -> Region.t -> t

(** Partial strategy that contains all possible strategies. *)
val all : unit -> t
(** Empty strategy: no action are possilbe. *)
val none : unit -> t
val disj : t list -> t
val conj : t list -> t
val to_bdd : t -> Cudd.bdd

val rename : t -> (string * string) list -> t

(** Takes a BDD representing the winning strategy and the controllable and uncontrollable inputs and returns individual BDDs to control the controllable inputs *)
val to_bdds : Cudd.bdd -> BddVariable.t list -> BddVariable.t list -> (BddVariable.t * Cudd.bdd) list

(** Takes the original aiger file, a BDD representing the winning strategy and the controllable and uncontrollable inputs *)
val to_aiger : AigerImperative.t -> t -> BddVariable.t list -> BddVariable.t list -> AigerImperative.t
