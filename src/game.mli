(** Parse an aiger file and return two lists, one containing the controllables inputs and the other the uncontrollable inputs. *)
val read_from_file : string -> (Aiger.t * Aiger.lit list * Aiger.lit list)

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

val of_aiger : aig:Aiger.t -> inputs:string list -> outputs:string list -> errors:string list -> t

(** The error of the product is the disjunction of the errors of each game *)
val product : t list -> t

(** Warning: we assume that the renaming does not rename inputs and all variables are Boolean *)
val rename : t -> (string * string) list -> t
