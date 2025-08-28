module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int = 
  {1 .. upperBound - 1}
  |> Seq.filter (fun n -> Seq.exists (fun d -> d <> 0 && n % d = 0) numbers)
  |> Seq.sum