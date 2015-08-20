
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

val reglisse_to_aiger : ?prefix:string -> ?environment:(string,Region.t) Hashtbl.t -> t -> Aiger.t

