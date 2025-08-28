module Isogram

let isIsogram (str: string) =
  let sanitizedWord = 
    str.ToLower()
    |> String.filter (fun c -> c >= 'a' && c <= 'z')
  sanitizedWord
  |> Set.ofSeq
  |> Set.count
  |> (=) (String.length sanitizedWord)