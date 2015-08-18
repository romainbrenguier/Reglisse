(** Values have the following meaning:
    * Winning -> there is a winning strategy
    * CoopWinning -> there is no winning strategy but we can win if other players cooperate
    * Losing -> it is impossible to win
    * NotWinning -> there is no winning strategy (union of the two preceding cases)
    * NotLosing -> it is possible to win (union of the two first cases)
    * Help -> state is not losing and not winning but the adversary can take two different actions which result in a state of value at least 0
 *)
type value = Winning | CoopWinning | Help | Losing | NotWinning | NotLosing

(** [value aiger controllables uncontrollables failure i] 
    Compute the set of states which have value [i].
    It returns a region containing the configurations of the latches that have value [i], and the pairs of configuration and action of the environment that have value [i].
    To avoid computing the same sets several time, you should first compute [value aiger controllables uncontrollables failure] then call this on the [i]'s you need.
    If weak is set to true then the controller has to provide its action before the environment.
*)
val value : AigerBdd.Circuit.t -> AigerBdd.Variable.t list -> AigerBdd.Variable.t list -> ?weak:bool -> Cudd.bdd -> value -> Region.t

(** Given an algorithm to compute winning states, gives an algorithm to compute the value *)
val make : (AigerBdd.Circuit.t -> Aiger.lit list -> Aiger.lit list -> Region.t) -> AigerBdd.Circuit.t -> Aiger.lit list -> Aiger.lit list -> value -> Region.t

val strategies : AigerBdd.Circuit.t -> (value -> Region.t) -> Region.t

(** The arguments are: the game, list of inputs controlled by player 1, list of inputs controlled by player 2, states to be avoided by player 1, states to be avoided by player 2. 
We should assume that player 2 choses its inputs before player 1. *)
val assume_admissible : AigerBdd.Circuit.t -> AigerBdd.Variable.t list -> AigerBdd.Variable.t list -> Cudd.bdd -> Cudd.bdd -> Region.t

type game = Aiger.t * AigerBdd.Circuit.t * AigerBdd.Variable.t list * AigerBdd.Variable.t list * Cudd.bdd
val admissible_strategies : game -> int * Region.t
val compositional_synthesis : game list -> Aiger.t * AigerBdd.Circuit.t * AigerBdd.VariableSet.t * Region.t

(** Given a strategy, computes the set of unsafe states.
    The function checks that the strategy is safe but not that it is a correct strategy (ie: at least one action is possible in each state)
*)
val check_strategies : AigerBdd.Circuit.t -> Cudd.bdd -> Aiger.lit list -> Aiger.lit list -> Cudd.bdd -> Cudd.bdd

(** The function checks that the strategy is a correct strategy (ie: at least one action is possible in each state)*)
val correct_strategy : AigerBdd.Circuit.t -> Cudd.bdd -> AigerBdd.Variable.t list -> AigerBdd.Variable.t list -> Cudd.bdd
