module NthPrime

let sqrt = double >> System.Math.Sqrt >> int

let isPrime = function
  | n when n < 2 -> false
  | n when n = 2 -> true
  | n when n % 2 = 0 -> false
  | n -> {3 .. 2 .. sqrt n} |> Seq.exists (fun div -> n % div = 0) |> not

let prime nth : int option = 
  match nth with
  | nth when nth < 1 -> None
  | nth -> Seq.initInfinite id |> Seq.filter isPrime |> Seq.take nth |> Seq.last |> Some