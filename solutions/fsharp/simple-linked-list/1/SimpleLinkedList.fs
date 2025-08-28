module SimpleLinkedList

//TODO: define LinkedList type
type LinkedList<'a> = 
  | Empty
  | Node of 'a * LinkedList<'a>

let nil = Empty

let create x n = Node (x, n)

let isNil x = match x with Empty -> true | _ -> false

let next x = match x with Empty -> Empty | Node (_, n) -> n

let datum x = match x with Empty -> failwith "Empty" | Node (d, _) -> d

let rec toList x = match x with Empty -> [] | Node (v, n) -> v :: toList n

let rec fromList xs = match xs with [] -> Empty | x :: xs -> Node (x, fromList xs)

let reverse x = x |> toList |> List.rev |> fromList
