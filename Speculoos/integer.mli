type t

val make : Boolean.t array -> t
val bool : bool -> t
val of_boolean : Boolean.t -> t
exception NonBoolean of t
val to_boolean : t -> Boolean.t
val to_string : t -> string
val of_array : bool array -> t
val to_boolean_array : t -> Boolean.t array
val int : int -> t
(** Gives the i-th bit of an expression *)
val get : t -> int -> Boolean.t
val get_expr : t -> int -> t
val size : t -> int
val var : string -> int -> t
val bool_var : string -> t

(** Select the bits given by the list, each pairs corresponding to an interval. *)
val select : t -> (int * int) list -> t

(** Bitwise negation. *)
val neg : t -> t 
(** Bitwise conjunction. *)
val conj : t -> t -> t
(** Bitwise disjunction. *)
val disj : t -> t -> t

(** Bitwise xor *)
val xor : t -> t -> t
(** Bitwise equality *)
val equiv : t -> t -> t
(** Bitwise implication *)
val implies : t -> t -> t



(** Equality. *)
val equals : t -> t -> Boolean.t

(** Addition *)
val add : t -> t -> t

val minus : t -> t -> t

val left_shift : t -> int -> t 

val right_shift : t -> int -> t 

val mult : t -> t -> t

(** [less a b] a is less than b (i.e. a < b)*)
val less : t -> t -> Boolean.t
val less_eq : t -> t -> Boolean.t

val greater : t -> t -> Boolean.t
val greater_eq : t -> t -> Boolean.t

(** Gives the quotient and the remainder in the integer division *)
val full_divide : t -> t -> (t * t)
val modulo : t -> t -> t
val div : t -> t -> t

(** Selects the element in the array that is indexed by the first argument *)
val multiplex : t -> t array -> t
(** If Then Else *)
val ite : Boolean.t -> t -> t -> t

(** Bitwise reductions *)
val andR : t -> Boolean.t
val orR : t -> Boolean.t
val xorR : t -> Boolean.t

val for_each : (int * int) list -> (int -> t) -> t

(** Find solutions to an equation.
    The second parameter is the list of parameters to find. *)
val solve : Boolean.t -> t list -> t list


