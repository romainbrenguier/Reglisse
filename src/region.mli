(** Regions of two player games *)

(** Type for regions of two-player games  *)
type t 

(** configurations of the latches (corresponds to player 1 states) *)
val latch_configuration : t -> Cudd.bdd
(** latches and inputs that are in the region (corresponds to player 2 states) *)
val latch_input_configuration : t -> Cudd.bdd

val negation : t -> t
val diff : t -> t -> t
val intersection : t -> t -> t
val intersection_bdd : t -> Cudd.bdd -> t
val union : t -> t -> t 
val equal : t -> t -> bool
(** everything *)
val tt : unit -> t
(** nothing *)
val ff : unit -> t 


val greatest_fixpoint : (t -> t) -> t -> t
val smallest_fixpoint : (t -> t) -> t -> t

(** initial state of the game (valuation 0 for latches *and* outputs) *)
val initial_state : Aiger.t -> Cudd.bdd


(** the first BDD gives restriction on the action of the players playing first and the second BDD on the actions of the player playing second *)
val of_bdds : Cudd.bdd -> Cudd.bdd -> t
