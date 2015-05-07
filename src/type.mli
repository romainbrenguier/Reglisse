(** Speculog types *)
type t = 
| Unit
| Bool
| Int of int
| Array of t * int
| Record of (string * t) list
| Union of (string * t) list

val unit : t
val bool : t
val int : int -> t
val array : t -> int -> t
val record : (string * t) list -> t
val union : (string * t) list -> t

val of_string : string -> t
val to_string : t -> string 

val keywords : string list
val lexer : char Stream.t -> Genlex.token Stream.t
val parse : Genlex.token Stream.t -> t

(** Number of bit to represent an element of that type *)
val size : t -> int
(** Special function for union type *)
val union_sizes : t -> int * int

(** List of constructors with the type of the argument and of the result. *)
val constructors : t -> (string * t * t) list



