module Pangram

let isPangram (input: string): bool =
  input.ToLower()
  |> Seq.filter (fun c -> c >= 'a' && c <= 'z')
  |> Set.ofSeq
  |> Seq.length
  |> (=) 26
