type t = Proposition.t list

val to_string : t -> string
val neg : t -> RegularExpression.t
val of_regexp : RegularExpression.t -> t
val of_string : string -> t
val string_neg : string -> RegularExpression.t
