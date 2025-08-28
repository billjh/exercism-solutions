module Grains

let square (n: int): Result<uint64,string> = 
  match n with
  | n when n < 1 || n > 64 -> Error "square must be between 1 and 64"
  | n                      -> Ok (bigint.Pow (2I, n - 1) |> uint64)

let total: Result<uint64,string> =
  let add a b = 
    match a, b with
    | Ok ai, Ok bi -> Ok (ai + bi)
    | _            -> Error "a and b must be Ok when add"
  {1 .. 64}
  |> Seq.map square
  |> Seq.reduce add
  
