module Raindrops

let convert (number: int): string = 
  let f div s = if div <> 0 && number % div = 0 then s else ""
  match f 3 "Pling" + f 5 "Plang" + f 7 "Plong" with
  | "" -> number |> string
  | s -> s
  