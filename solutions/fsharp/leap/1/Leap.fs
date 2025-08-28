module Leap

let leapYear (year: int): bool = 
  let divisibleBy num =
    match num with
    | 0 -> false
    | _ -> year % num = 0
  divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))