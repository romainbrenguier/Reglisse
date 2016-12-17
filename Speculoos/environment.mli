(** environment containing module definitions *)
type env

val empty_env : env
val add_module : env -> string -> Aiger.t -> env
val find_module : env -> string -> Aiger.t
val add_parametric_module : env -> string -> ParametricModule.t -> env
val find_parametric_module : env -> string -> ParametricModule.t

val merge_env : env -> env -> env

(** Environment destined to be modified by dynamically loaded modules. *)
val current_env : (unit -> env) ref 


