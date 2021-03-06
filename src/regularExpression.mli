(** Type of regular expressions *)
type t = 
| Prop of Proposition.t 
| Alt of t list
| Concat of t * t
| Star of t
| Epsilon
| Empty

(** The grammar for regular expression is given by : 
r ::= r | r ; r & r ; r* ; r r; (r); { p }; r+ ; r?; .
p ::= p | p ; p & p ; !p ; p <-> p; (p); ident ; true

For conjunction and negation see the extendedExpression module.
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
val labels : t -> ReglisseCommon.StringSet.t

val to_string : t -> string

(** [to_aiger] and [regexp expr] convert a regular expression 
    into an AIG with an output called (prefix)_accept that is true after seeing 
    an accepting state.
    Cudd has to be initialized before calling this function.
*)
val to_aiger : ?prefix:string -> t -> AigerImperative.t
val regexp : string -> AigerImperative.t

val test : string -> unit
