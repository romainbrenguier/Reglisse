(** Speculoos expressions *)

(** Type of expressions *)
type t = 
| EUnit
| EBool of Boolean.t 
| EInt of Integer.t
| EArray of t array
| ERecord of (string * t) list
| EConstr of (string * t)

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
val to_integer : t -> Integer.t

(** Should only be applied to arrays.
    The first argument is a table and
    the second argument is the index of the element we want. *)
val get : t -> t -> t
(** Infix notation for get *)
val ($@) : t -> t -> t
val size : t -> int



(** Should only be applied to tuples *)
val field : t -> string -> t
(** infix notation for field *)
val ($.) : t -> string -> t
(** Should only be applied to integers 
    Select the bits given by the list, each pairs corresponding to an interval. *)
val select : t -> (int * int) list -> t

(** Should only be applied to union types *)
val match_with : Type.t -> t -> (string * (t -> t)) list -> t
val match_case : Type.t -> t -> string -> Type.t * t

val to_type : t -> Type.t

(** {2 Operations} *)
(* Most of them are duplicates of integer functions. *)
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
val ite : t -> t -> t -> t

(** Selects the element in the array that is indexed by the first argument *)
val mux : t -> t array -> t

(** Bitwise reductions *)
val andR : t -> t
val orR : t -> t
val xorR : t -> t


(* val apply : (unit Integer.t -> unit Integer.t -> unit Integer.t) -> t -> t -> t*)
(** Should be applied to Boolean expressions *)
val for_each : (int * int) list -> (int -> t) -> t
val for_some : (int * int) list -> (int -> t) -> t

(** {2 Conversions} *)
val to_string : t -> string 
val to_int : Type.t -> t -> t
val of_int : Type.t -> t -> t


