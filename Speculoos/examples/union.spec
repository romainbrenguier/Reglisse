var position : Pos of int 7 | Neg of int 7 ;
var input : Left | Right ;
var value : int 2;
var output :  int 7;
var output_sign : bool;

position <- 
  match input with
  | Left -> (match position with
    | Pos i -> (i > 0) ? Pos (i - 1) : Neg 1
    | Neg i -> Neg (i + 1))
  | Right -> (match position with
    | Pos i -> Pos (i + 1) 
    | Neg i -> (i > 0) ? Neg (i - 1) : Pos 1);

output <- match position with | Pos x -> x | Neg x -> x;
output_sign <- match position with | Pos x -> false | Neg x -> true;
