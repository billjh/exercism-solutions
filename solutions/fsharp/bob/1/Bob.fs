module Bob

type SentenceEnd = 
  | Question
  | Exclamation
  | Period
  | Others

let (|Postfix|_|) (p:string) (input:string) =
  match input.EndsWith(p) with
  | true  -> Some input
  | false -> None

let getSentenceEnd (input:string): SentenceEnd = 
  match input.TrimEnd() with
  | Postfix "?" _ -> Question
  | Postfix "!" _ -> Exclamation
  | Postfix "." _ -> Period
  | _             -> Others

let isYelling (input:string): bool =
  let letters = input |> Seq.filter (fun c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')
  Seq.isEmpty letters |> not && Seq.forall (fun c -> c >= 'A' && c <= 'Z') letters

let hasContent (input:string): bool =
  input.Trim() <> ""

let response (input: string): string =
  match getSentenceEnd input, isYelling input with
  | Question,    true  -> "Calm down, I know what I'm doing!"
  | Question,    false -> "Sure."
  | _,           true  -> "Whoa, chill out!"
  | _ -> 
    match hasContent input with
    | false -> "Fine. Be that way!"
    | true  -> "Whatever."

