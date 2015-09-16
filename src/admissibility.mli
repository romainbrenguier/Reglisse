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
val value : ?weak:bool -> Game.t -> value -> Region.t

(** States from which there is no hope of winning. *)
val losing : Game.t -> Region.t

val strategies : AigerBdd.Circuit.t -> (value -> Region.t) -> Strategy.t

val admissible_strategies : Game.t -> int * Strategy.t
val compositional_synthesis : Game.t list -> Aiger.t * AigerBdd.Circuit.t * AigerBdd.VariableSet.t * Region.t

