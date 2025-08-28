module DifferenceOfSquares

let squareOfSum (number: int): int = 
  {1 .. number} |> Seq.sum |> fun x -> x * x

let sumOfSquares (number: int): int = 
  {1 .. number} |> Seq.sumBy (fun x -> x * x)

let differenceOfSquares (number: int): int = 
  squareOfSum number - sumOfSquares number