open RegularExpression

type t = Proposition.t list

let to_string seq = 
  let seq = 
    List.map (fun x -> "{"^Proposition.to_string x^"}") seq 
  in
  Common.list_to_string seq " "

let neg seq = List.map (fun x -> Proposition.neg x) seq

let of_regexp r = 
  let rec aux accu = function 
    | Prop p -> p :: accu
    | Concat (x,y) -> aux (aux accu x) y
    | Epsilon -> accu
    | Alt [a] -> aux accu a
    | Empty -> []
    | Alt _ -> failwith "in Sequence.of_regexp: no alternation allowed in sequences"
    | Star _ -> failwith "in Sequence.of_regexp: no star allowed in sequences"
  in 
  List.rev (aux [] r)

let of_string s = 
  of_regexp (RegularExpression.of_string s)

let string_neg s = 
  to_string (neg (of_string s))
