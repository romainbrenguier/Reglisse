let log i = 
  let rec aux i =
    if i < 2 then 1
    else 1 + aux (i/2)
  in aux i

let exp i =
  let rec aux i =
    if i < 1 then 1
    else 2 * aux (i-1)
  in aux i

let iter start last f x = 
  let rec aux i accu = 
    if i > last then accu else aux (i+1) (f i accu)
  in aux start x

let next_token stream =
  (match Stream.next stream with
  | Genlex.Kwd x-> x
  | Genlex.Ident x -> x
  | Genlex.String x -> x
  | Genlex.Float f -> string_of_float f
  | Genlex.Int i -> string_of_int i
  | Genlex.Char c -> "char")


let remaining_tokens stream =
  let buf = Buffer.create 100 in
  let rec loop () = 
    Printf.bprintf buf "%s " (next_token stream);
    loop ()
  in
  try loop () with _ ->  Buffer.contents buf
   

let display_infos = ref false
let display_debug = ref false
let display_warnings = ref false


let infos string = 
  if !display_infos then print_endline string

let debug string =
  if !display_debug then print_endline string
  
let warning string = 
  if !display_warnings then print_endline string

let starts_with prefix string = 
  if String.length string >= String.length prefix 
  then String.sub string 0 (String.length prefix) = prefix
  else false


let list_to_string list separator =
  match list with 
  | [] -> ""
  | hd :: tl -> List.fold_left (fun accu s -> accu^separator^s) hd tl


let tmp_name_count = ref 0
let tmp_name () =
  incr tmp_name_count;
  "_tmp_"^string_of_int !tmp_name_count
