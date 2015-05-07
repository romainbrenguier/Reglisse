type t = 
  | EVar of string * int
  | ENext of string * int
  | EExists of t list * t
  | EForall of t list * t
  | ENot of t
  | EAnd of t list
  | EOr of t list
  | EList of t list
  | True 
  | False

val to_string : t -> string
val var : string -> int -> t
val next_var : string -> int -> t
exception AlreadyNext of t
val next : t -> t
    
val for_each : (int * int) list -> (int -> t) -> t
val of_bdd : Cudd.bdd -> AigerBdd.symbol list -> t
val conjunction : t -> t -> t
val disjunction : t -> t -> t
val negation : t -> t 
val xor : t -> t -> t
val equality : t -> t -> t
val implication : t -> t -> t
val forall : t list -> t -> t
val exists : t list -> t -> t
val of_list : t list -> t


