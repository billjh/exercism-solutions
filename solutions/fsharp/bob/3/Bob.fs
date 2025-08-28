module Bob

let (|Question|_|) (input:string) =
  if input.TrimEnd().EndsWith("?") then Some() else None

let (|Yelling|_|) (input:string) =
  if input.ToLower() <> input && input.ToUpper() = input then Some() else None

let (|WithContent|_|) (input:string) =
  if input.Trim() <> "" then Some() else None

let response (input: string): string =
  match input with
  | Question & Yelling -> "Calm down, I know what I'm doing!"
  | Question -> "Sure."
  | Yelling -> "Whoa, chill out!"
  | WithContent -> "Whatever."
  | _ -> "Fine. Be that way!"

