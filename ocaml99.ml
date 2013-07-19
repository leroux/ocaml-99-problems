let rec last = function
  | []    -> None
  | [x]   -> Some x
  | _::xs -> last xs

let rec last_two = function
  | []     -> None
  | [x; y] -> Some (x, y)
  | _::xs  -> last_two xs

let rec at k = function
  | []    -> None
  | x::xs -> if k = 1 then x else at (k - 1) xs

let length l =
  let rec length' n = function
    | []    -> n
    | _::xs -> length' (n + 1) xs in
  length' 0 l

let reverse l =
  let rec reverse' acc = function
  | []    -> acc
  | x::xs -> reverse' (x::acc) xs in
  reverse' [] l

let is_palindrome l = reverse l = l

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten xs =
  let rec flatten' acc = function
    | []           -> acc
    | One x :: xs  -> flatten' (x :: acc) xs
    | Many x :: xs -> flatten' (flatten' acc x) xs in
  reverse (flatten' [] xs)


