(** Atomic propositions. 
   Talk about proposition that are true in one state *)
type t = Var of string * int | And of t * t | Or of t * t | Not of t | True | False

val var : string -> int -> t
val conj : t -> t -> t
val disj : t -> t -> t
val neg : t -> t

val to_string : t -> string 

val compare : t -> t -> int

val to_speculog : t -> string

val to_bdd : t -> Cudd.bdd

(** the second argument is a maping from variables appearing in the proposition to speculog variables *)
val to_expression : t -> (string * Expression.t) list -> Expression.t

val labels : t -> ReglisseCommon.StringSet.t

val parse_keywords : string list
val parse : Genlex.token Stream.t -> t
