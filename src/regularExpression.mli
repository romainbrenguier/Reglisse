module StringSet : Set.S with type elt = string

(* Atomic propositions. 
   Talks about proposition that are true in one state *)
module Proposition : 
sig
  type t = Var of string * int | And of t * t | Or of t * t | Not of t | True | False
  val to_string : t -> string 
  val compare : t -> t -> int
  val to_speculog : t -> string
  val to_bdd : t -> Cudd.bdd
(** the second argument is a maping from variables appearing in the proposition to speculog variables *)
  val to_expression : t -> (string * Expression.t) list -> Expression.t
  val labels : t -> StringSet.t
end


(** Type of regular expressions *)
type t

(** The grammar for regular expression is given by : 
r ::= r | r ; r & r ; r* ; r r; (r); { p }; r+ ; r?; .
p ::= p | p ; p & p ; !p ; (p); ident ; true
*)
val of_string : string -> t

(** Alternative between two expressions *)
val disj : t -> t -> t
val alt : t list -> t
(** Concatenation *)
val concat : t -> t -> t
val seq : t list -> t
(** Repetition *)
val star : t -> t
val plus : t -> t
(** [times e n] repeat [n] times expression [e] *)
val times : t -> int -> t
(** Optional expression *)
val opt : t -> t
(** Proposition *)
val prop : Proposition.t -> t

(** Labels appearing in the expression *)
val labels : t -> StringSet.t

val to_string : t -> string

(** [to_aiger] and [regexp expr] convert a regular expression 
    into an AIG with an output called ACCEPT that is true after seeing 
    an accepting state and a variable RESIDUAL that encodes
    the current state.
*)
val to_aiger : ?prefix:string -> t -> Aiger.t
val regexp : string -> Aiger.t

val test : string -> unit
