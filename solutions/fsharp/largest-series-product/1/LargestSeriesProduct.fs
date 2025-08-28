module LargestSeriesProduct

let (|AnyNonDigit|_|) input =
  if Seq.forall System.Char.IsDigit input |> not then Some() else None

let (|ShorterThan|_|) threshold input =
  if String.length input < threshold then Some() else None

let (|Negative|Zero|Positive|) = function
  | 0 -> Zero
  | n when n < 0 -> Negative
  | _ -> Positive

let largestProduct input seriesLength : int option =
  match input, seriesLength with
  | AnyNonDigit, _ -> None
  | ShorterThan seriesLength, _ -> None
  | _, Negative -> None
  | _, Zero -> Some 1
  | _, Positive ->
    input
    |> Seq.map (fun c -> c.ToString() |> int)
    |> Seq.windowed seriesLength
    |> Seq.map (Seq.reduce (*))
    |> Seq.max
    |> Some