(** Speculog expressions *)

(** Type of expressions *)
type t 

(*(** Declarations form type *)
val input : string -> Type.t -> Speculog.declaration * t 
val output : string -> Type.t -> Speculog.declaration * t 
val reg : string -> Type.t -> Speculog.declaration * t 
val wire : string -> Type.t -> Speculog.declaration * t *)
(** Variable declaration *)
val var : string -> Type.t -> t

(** {2 Constructors} *)
val unit : t
val bool : bool -> t
val int : int -> t
val array : t array -> t
val record : (string * t) list -> t
val constr : Type.t -> string -> t -> t


(** {2 Access} *)

(** Should only be applied to simple types (bool and int) *)
val to_expr : t -> unit Integer.t

(** Should only be applied to arrays.
    The first argument is a table and
    the second argument is the index of the element we want. *)
val get : t -> t -> t
val size : t -> int

(** Should only be applied to tuples *)
val field : t -> string -> t

(** Should only be applied to union types *)
val match_with : Type.t -> t -> (string * (t -> t)) list -> t
val match_case : Type.t -> t -> string -> Type.t * t

val to_type : t -> Type.t

(** {2 Operations} *)
val neg : t -> t
val implies : t -> t -> t
val equiv : t -> t -> t
val conj : t -> t -> t
val disj : t -> t -> t
val xor : t -> t -> t
val equals : t -> t -> t
val less_eq : t -> t -> t
val less : t -> t -> t
val greater_eq : t -> t -> t
val greater : t -> t -> t
val left_shift : t -> int -> t
val right_shift : t -> int -> t 
val add : t -> t -> t
val minus : t -> t -> t
val mult : t -> t -> t
val div : t -> t -> t
val modulo : t -> t -> t
val next : t -> t

val ite : t -> t -> t -> t

(* val apply : (unit Integer.t -> unit Integer.t -> unit Integer.t) -> t -> t -> t*)
(** Should be applied to Boolean expressions *)
val for_each : (int * int) list -> (int -> t) -> t
val for_some : (int * int) list -> (int -> t) -> t

(** {2 Conversions} *)
val to_string : t -> string 
val to_int : Type.t -> t -> t
val of_int : Type.t -> t -> t
  
(** {2 Synthesis } *)

val functional_synthesis : (t * t) list -> Aiger.t
(** Add an initial configuration to the specifications. *)
val init : (t * t) list -> (t * t) list -> (t * t) list


val rename : Aiger.t -> string -> Type.t -> string -> Aiger.t
val to_symbols : Aiger.t -> t -> Aiger.Symbol.t list

(** [import_module aig [a1,b1;...;an,bn] gen] import a module
    renaming variables [ai] into [bi], the list of newly 
    created output variables is given as argument to gen, to generate
    the final AIG. In the end outputs are hidden. *)
val use_module : Aiger.t -> inputs:((string * t) list) -> outputs:((string * string) list) -> (t list -> Aiger.t) -> Aiger.t

val make_latch : string -> Type.t -> (t -> t -> Aiger.t) -> Aiger.t
