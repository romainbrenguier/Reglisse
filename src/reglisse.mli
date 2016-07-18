type safety = 
  | Not of string 
  | Never of string 
  | If_then of string * string 
  | Only_if of string * string
  | When of string * string 
  | If_then_else of string * string * string 
  | When_else of string * string * string 

val safety_to_expr : safety -> ExtendedExpression.t

module Program :
sig
  type t 
  val skip : t
  val prop : Proposition.t -> t
  val concat : t -> t -> t
  val make_while : Proposition.t -> t -> t
  val if_then_else : Proposition.t -> t -> t -> t
  val to_extended_expression : t -> RegularExpression.t
end


type module_content = 
| Calls of (string * string list) list
| Safety of safety list
| Eventually of Program.t list
| Functional of (string * Expression.t) list * (Expression.t * Expression.t) list
| Empty

type t = 
  { 
    module_name: string;
    inputs: string list; 
    outputs: string list;
    content: module_content;
  }

val new_module : string -> t
val add_input : t -> string -> t
val add_output : t -> string -> t
(** Add a safety condition. Only for atomic modules. *)
val add_safety : t -> safety -> t
(** Add a call. Only for compositional modules. *)
val add_call : t -> (string * string list) -> t
(** Add an update. Only for functional modules. *)
val add_update : t -> (Expression.t * Expression.t) -> t

(** return true if the module does not contain calls *)
val is_atomic : t -> bool

val lexer : char Stream.t -> Genlex.token Stream.t

exception NoMoreModule

val parse : Genlex.token Stream.t -> t
val parse_file : string -> t list


val safety_to_aiger : ?prefix:string -> t -> AigerImperative.t option

(** Construct a game with prefix given by the module name *)
val safety_to_game : t -> Game.t option


val functional_to_aiger : t -> AigerImperative.t option

module Env :
sig 
  type t
  val create : int -> t
  val new_module : t -> string -> string list -> string list -> unit
  val add_aiger : t -> string -> AigerImperative.t -> unit
  val add_game : t -> string -> Game.t -> unit
  (** Functions with name ending with exn may throw a Not_found exception *)
  val find_arguments_exn : t -> string -> string list 
  val find_arguments : t -> string -> (string list) option
  val find_aiger_exn : t -> string -> AigerImperative.t
  val find_aiger : t -> string -> AigerImperative.t option
  val find_game_exn : t -> string -> Game.t
  val find_game : t -> string -> Game.t option
end

(** When the environment contains an implementation for each call we can construct 
 *  an implementation of the compositional module. *)
val calls_to_aiger : ?env:Env.t -> t -> AigerImperative.t option

type call_renaming = {call:string; renaming:(string * string) list}

val calls_to_game : Env.t -> (string,t) Hashtbl.t -> t -> (Game.t * (call_renaming list)) option
