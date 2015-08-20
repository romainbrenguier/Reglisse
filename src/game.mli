(** Parse an aiger file and return two lists, one containing the controllables inputs and the other the uncontrollable inputs. *)
val read_from_file : string -> (Aiger.t * Aiger.lit list * Aiger.lit list)
val of_aiger : Aiger.t -> (Aiger.t * Aiger.lit list * Aiger.lit list)

val controllable_name : string -> bool
(** List of controllables and uncontrollables inputs in the AIGER file *)
val controllables : Aiger.t -> (Aiger.lit list * Aiger.lit list)
val controllable_variables : Aiger.t -> (AigerBdd.variable list * AigerBdd.variable list)
(** Gives the list of controllable and uncontrollable variables *)
val control : Aiger.t -> string list -> string list -> AigerBdd.Variable.t list * AigerBdd.Variable.t list


type t = {
  aiger:Aiger.t; 
  circuit: AigerBdd.Circuit.t; 
  contr: AigerBdd.Variable.t list;
  uncontr: AigerBdd.Variable.t list;
  err: Cudd.bdd
}


val initial_state : t -> Cudd.bdd

val of_aiger : Aiger.t -> string list -> string list -> string -> t
