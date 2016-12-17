type t
val compare : t -> t -> int
val find : string -> t
val to_bdd : t -> Cudd.bdd
val next : t -> t
val make_cube : t list -> Cudd.cube
val to_int : t -> int
val of_int : int -> t
val to_string : t -> string
val of_lit_exn : AigerImperative.t -> AigerImperative.lit -> t 
val max_var : unit -> int
val rename_configuration : Cudd.bdd -> t array -> t array -> Cudd.bdd
  
