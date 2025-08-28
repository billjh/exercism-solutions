module Bob

let (|Question|_|) (input:string) =
  match input.TrimEnd().EndsWith("?") with
  | true -> Some()
  | false -> None

let (|Yelling|_|) (input:string) =
  match input.ToLower() <> input && input.ToUpper() = input with
  | true -> Some()
  | false -> None

let (|WithContent|_|) (input:string) =
  match input.Trim() <> "" with
  | true -> Some()
  | false -> None

let response (input: string): string =
  match input with
  | Question & Yelling -> "Calm down, I know what I'm doing!"
  | Question -> "Sure."
  | Yelling -> "Whoa, chill out!"
  | WithContent -> "Whatever."
  | _ -> "Fine. Be that way!"

