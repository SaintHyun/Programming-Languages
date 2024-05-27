module P5

/// For int list 'l' that contains decimal digits (0~9), return the integer that
/// is represented by this list. For example, "digitsToInt [1; 3; 2]" must
/// return 132 as a result. When the input list is empty, just return 0.
let rec reverse (l: List<'a>) : List<'a> =
  match l with 
    | [] -> []
    | head :: tail -> (reverse tail) @ [head]

let rec ans (a: int list) : int = 
    match a with
    | [] -> 0
    | head :: tail -> head + 10 * ans tail

let rec digitsToInt (l: int list) : int =
  let a = reverse l
  ans a