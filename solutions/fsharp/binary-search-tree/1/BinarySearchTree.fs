module BinarySearchTree

type TreeNode<'a> = { left:TreeNode<'a> option; data:'a; right:TreeNode<'a> option }

let left node = node.left

let right node = node.right

let data node = node.data

let createNode data = { TreeNode.data = data; left = None; right = None }

let rec insert node data =
  if data <= node.data
  then
    match node.left with
    | None -> { node with left = Some (createNode data) }
    | Some left' -> { node with left = Some (insert left' data) }
  else
    match node.right with
    | None -> { node with right = Some (createNode data) }
    | Some right' -> { node with right = Some (insert right' data) }

let create items =
  match items with
  | [] -> failwith "items cannot be empty"
  | head::ns -> List.fold insert (createNode head) ns

let rec sortedData node = 
  let sortedData' = Option.map sortedData >> Option.defaultValue []
  (sortedData' node.left) @ [node.data] @ (sortedData' node.right)
