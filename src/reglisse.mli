type safety = 
  | Not of string 
  | Never of string 
  | If_then of string * string 
  | Only_if of string * string
  | When of string * string 
  | If_then_else of string * string * string 
  | When_else of string * string * string 

val safety_to_expr : safety -> ExtendedExpression.t

type t = 
  { 
    module_name: string;
    inputs: string list; 
    outputs: string list;
    safety: safety list;
    eventually: string list;
    module_calls: (string * string list) list
  }

val new_module : string -> t
val add_input : t -> string -> t
val add_output : t -> string -> t
val add_safety : t -> safety -> t
val add_call : t -> (string * string list) -> t

val lexer : char Stream.t -> Genlex.token Stream.t

exception NoMoreModule

val parse : Genlex.token Stream.t -> t
val parse_file : string -> t list


val safety_to_aiger : ?prefix:string -> t -> Aiger.t option

(** Construct a game with prefix given by the module name *)
val safety_to_game : t -> Game.t option

module Env :
sig 
  type t
  val create : int -> t
  val new_module : t -> string -> string list -> string list -> unit
  val add_aiger : t -> string -> Aiger.t -> unit
  val add_game : t -> string -> Game.t -> unit
  (** Functions with name ending with exn may throw a Not_found exception *)
  val find_arguments_exn : t -> string -> string list 
  val find_arguments : t -> string -> (string list) option
  val find_aiger_exn : t -> string -> Aiger.t
  val find_aiger : t -> string -> Aiger.t option
  val find_game_exn : t -> string -> Game.t
  val find_game : t -> string -> Game.t option
end

(** When the environment contains an implementation for each call we can construct 
 *  an implementation of the compositional module. *)
val calls_to_aiger : ?env:Env.t -> t -> Aiger.t option


val calls_to_game : Env.t -> t -> Game.t option



