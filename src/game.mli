(** Parse an aiger file and return two lists, one containing the controllables inputs and the other the uncontrollable inputs. *)
val read_from_file : string -> (AigerImperative.t * AigerImperative.lit list * AigerImperative.lit list)

val controllable_name : string -> bool
(** List of controllables and uncontrollables inputs in the AIGER file *)
val controllables : AigerImperative.t -> (AigerImperative.lit list * AigerImperative.lit list)
val controllable_variables : AigerImperative.t -> (BddVariable.t list * BddVariable.t list)
(** Gives the list of controllable and uncontrollable variables *)
val control : AigerImperative.t -> string list -> string list -> BddVariable.t list * BddVariable.t list


type t = {
  aiger:AigerImperative.t; 
  circuit: Circuit.t; 
  contr: BddVariable.t list;
  uncontr: BddVariable.t list;
  err: Cudd.bdd
}


val initial_state : t -> Cudd.bdd

val of_aiger : aig:AigerImperative.t -> inputs:string list -> outputs:string list -> errors:string list -> t

(** The error of the product is the disjunction of the errors of each game *)
val product : t list -> t

(** Warning: we assume that the renaming does not rename inputs and all variables are Boolean *)
val rename : t -> (string * string) list -> t
