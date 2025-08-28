module IsbnVerifier

let isValid (isbn:string) =
  let nums = isbn |> Seq.filter (fun c -> System.Char.IsDigit c || c = 'X')
  (Seq.length nums = 10) 
  && 
  (nums |> Seq.take 9 |> Seq.forall System.Char.IsDigit) 
  &&
  (nums 
  |> Seq.map (function 'X' -> 10 | c -> c.ToString() |> int)
  |> Seq.zip {10 .. -1 .. 1}
  |> Seq.sumBy (fun (a, b) -> a * b)) 
  |> (%) <| 11
  |> (=) 0
