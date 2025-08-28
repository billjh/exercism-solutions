module Proverb

let recite (input: string list): string list =
  if List.isEmpty input 
  then []
  else input
    |> List.pairwise
    |> List.map (fun pair -> sprintf "For want of a %s the %s was lost." (fst pair) (snd pair))
    |> List.append
    <| [sprintf "And all for the want of a %s." (List.head input)]