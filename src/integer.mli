type 'a t
type s_bool = bool t
type s_int = int t


val make : 'a option -> Boolean.t array -> 'a t
val cast : 'a t -> 'b t
val bool : bool -> 'a t
val of_boolean : Boolean.t -> 'a t
exception NonBoolean of unit t
val to_boolean : bool t -> Boolean.t
val to_string : 'a t -> string
val of_array : bool array -> 'a t
val to_boolean_array : 'a t -> Boolean.t array
val int : int -> 'a t
(** Gives the i-th bit of an expression *)
val get : 'a t -> int -> Boolean.t
val get_expr : 'a t -> int -> 'a t
val size : 'a t -> int
val var : string -> int -> 'a t


(** Bitwise negation. *)
val neg : 'a t -> 'a t 
val not : 'a t -> 'a t 
val ( !! ) : 'a t -> 'a t
(** Bitwise conjunction. *)
val conj : 'a t -> 'a t -> 'a t
val ( && ) : 'a t -> 'a t -> 'a t
(** Bitwise disjunction. *)
(*   "|" cannot be used since it is used in caml for pattern matching *)
val disj : 'a t -> 'a t -> 'a t
val ( or ) : 'a t -> 'a t -> 'a t

(** Bitwise xor *)
val xor : 'a t -> 'a t -> 'a t
(** Another notation for [xor] *)
val ( ^^ ) : 'a t -> 'a t -> 'a t
(** Bitwise equality *)
val equiv : 'a t -> 'a t -> 'a t
(** Bitwise implication *)
val implies : 'a t -> 'a t -> 'a t



(** Equality. *)
(*"=" cannot be used since it is already used in caml *)
val equals : 'a t -> 'a t -> 'a t
val ( == ) : 'a t -> 'a t -> 'a t


val next : 'a t -> 'a t
 
(** Addition *)
val add : 'a t -> 'a t -> 'a t
val ( ++ ) : 'a t -> 'a t -> 'a t

val minus : 'a t -> 'a t -> 'a t
val ( -- ) : 'a t -> 'a t -> 'a t

val left_shift : 'a t -> int -> 'a t 

val right_shift : 'a t -> int -> 'a t 

val mult : 'a t -> 'a t -> 'a t

(** [less a b] a is less than b (i.e. a < b)*)
val less : 'a t -> 'a t -> 'a t
val less_eq : 'a t -> 'a t -> 'a t

val greater : 'a t -> 'a t -> 'a t
val greater_eq : 'a t -> 'a t -> 'a t

(** Gives the quotient and the remainder in the integer division *)
val full_divide : 'a t -> 'a t -> ('a t * 'a t)
val modulo : 'a t -> 'a t -> 'a t
val div : 'a t -> 'a t -> 'a t

(** Selects the element in the array that is indexed by the first argument *)
val multiplex : 'a t -> ('a t) array -> 'a t
(** If Then Else *)
val ite : bool t -> 'a t -> 'a t -> 'a t

val for_each : (int * int) list -> (int -> bool t) -> bool t

(** Find solutions to an equation.
    The second parameter is the list of parameters to find. *)
val solve : bool t -> 'a t list -> 'a t list


