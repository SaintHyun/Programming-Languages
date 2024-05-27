module P6

/// From list 'l', find the element that appears most frequently in the list,
/// and return how many times it appears. If the input list is empty, return 0.

let rec counting (l: List<'a>) (n: 'a): int = 
  match l with
  | [] -> 0
  | head :: tail -> 
  if head = n then 1 + (counting tail n) 
  else (counting tail n)

let countMostFrequent (l: List<'a>) : int =
  let rec helper l currentMax =
    match l with
    | [] -> currentMax
    | head :: tail -> 
      let headCount = counting l head
      let newMax = if headCount > currentMax then headCount else currentMax
      helper tail newMax
  helper l 0
