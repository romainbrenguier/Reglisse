(** {2 Speculoos expressions} *)


(** {3 Variable declaration} *)

(** A variable is declared by its named and its type. *)
val var : string -> Type.t -> Expression.t

(** {3 Constructors} *)

val unit : Expression.t
val bool : bool -> Expression.t
val int : int -> Expression.t
val array : Expression.t array -> Expression.t
val record : (string * Expression.t) list -> Expression.t
val constr : Type.t -> string -> Expression.t -> Expression.t

(** {3 Operators} *)

(** Bitwise negation *)
val neg : Expression.t -> Expression.t

(** Implication *)
val ($=>) : Expression.t -> Expression.t -> Expression.t

(** Equivalence *)
val ($<=>) : Expression.t -> Expression.t -> Expression.t
val ($&) : Expression.t -> Expression.t -> Expression.t
val ($^) : Expression.t -> Expression.t -> Expression.t
val ($|) : Expression.t -> Expression.t -> Expression.t
val ($=) : Expression.t -> Expression.t -> Expression.t
val ($<=) : Expression.t -> Expression.t -> Expression.t
val ($<) : Expression.t -> Expression.t -> Expression.t
val ($>=) : Expression.t -> Expression.t -> Expression.t
val ($>) : Expression.t -> Expression.t -> Expression.t
val ( $<< ) : Expression.t -> int -> Expression.t
val ( $>> ) : Expression.t -> int -> Expression.t
val ($+) : Expression.t -> Expression.t -> Expression.t
val ($-) : Expression.t -> Expression.t -> Expression.t
val ($* ) : Expression.t -> Expression.t -> Expression.t
val ($/) : Expression.t -> Expression.t -> Expression.t
val ($%) : Expression.t -> Expression.t -> Expression.t
val ite : Expression.t -> Expression.t -> Expression.t -> Expression.t
val ($?) : Expression.t -> (Expression.t * Expression.t) -> Expression.t
val mux : Expression.t -> Expression.t array -> Expression.t
(** Bitwise reductions *)
val andR : Expression.t -> Expression.t
val orR : Expression.t -> Expression.t
val xorR : Expression.t -> Expression.t

(** {2 Overriding Ocaml syntax} *)
module Syntax :
sig
  val (=>) : Expression.t -> Expression.t -> Expression.t
  val (<=>) : Expression.t -> Expression.t -> Expression.t
  val (&) : Expression.t -> Expression.t -> Expression.t
  val (^) : Expression.t -> Expression.t -> Expression.t
  val ( || ) : Expression.t -> Expression.t -> Expression.t
  val (==) : Expression.t -> Expression.t -> Expression.t
  val (<=) : Expression.t -> Expression.t -> Expression.t
  val (<) : Expression.t -> Expression.t -> Expression.t
  val (>=) : Expression.t -> Expression.t -> Expression.t
  val (>) : Expression.t -> Expression.t -> Expression.t
  val ( << ) : Expression.t -> int -> Expression.t
  val ( >> ) : Expression.t -> int -> Expression.t
  val (+) : Expression.t -> Expression.t -> Expression.t
  val (-) : Expression.t -> Expression.t -> Expression.t
  val ( * ) : Expression.t -> Expression.t -> Expression.t
  val (/) : Expression.t -> Expression.t -> Expression.t
  val (%) : Expression.t -> Expression.t -> Expression.t
end

(** {2 Instructions} *)
(** Instruction tree *)
type t = 
| Update of (Expression.t * Expression.t)
| When of (Expression.t * t)
| If of (Expression.t * t * t)
(* Warning: init should not be inside if or when constructs or it will be ignored. *)
| Init of (Expression.t * Expression.t)
| Seq of t list

val empty : t
val add_update : t -> Expression.t -> Expression.t -> t
val add_when : t -> Expression.t -> t -> t
val add_if : t -> Expression.t -> t -> t -> t
val add_init : t -> Expression.t -> Expression.t -> t
val seq : t list -> t

(** {2 Synthesis } *)

val to_aiger : t -> Aiger.t
val to_aig_imp : t -> AigerImperative.t

(** If no filename is provided the aiger file is produced on the standard output *)
val compile : ?filename:string -> t -> unit

(** [import_module aig [a1,b1;...;an,bn] gen] import a module
    renaming variables [ai] into [bi], the list of newly 
    created output variables is given as argument to gen, to generate
    the final AIG. In the end outputs are hidden. *)

(** Not implemented yet because compose is still missing from AigerImperative *)
(*val use_module : Aiger.t -> inputs:((string * Expression.t) list) -> outputs:((string * string) list) -> (Expression.t list -> Aiger.t) -> Aiger.t
*)
