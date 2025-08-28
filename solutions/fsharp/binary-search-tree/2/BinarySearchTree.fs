module BinarySearchTree

type Node<'a> = Node of Node<'a> option * 'a * Node<'a> option

let left (Node (l, _, _)) = l

let right (Node (_, _, r)) = r

let data (Node (_, d, _)) = d

let createNode data = Node (None, data, None)

let create items =
  let rec insert (Node (l, d, r)) data =
    let insert' = function 
      | None -> Some (createNode data) 
      | Some n -> Some (insert n data)
    if data <= d
    then Node (insert' l, d, r)
    else Node (l, d, insert' r)

  match items with
  | [] -> failwith "items cannot be empty"
  | n :: ns -> List.fold insert (createNode n) ns

let rec sortedData node = 
  let sort = Option.map sortedData >> Option.defaultValue []
  sort (left node) @ [data node] @ sort (right node)
