module RnaTranscription

let toRna (dna: string): string = 
  let trans = function
    | 'G' | 'g' -> Some 'C'
    | 'C' | 'c' -> Some 'G'
    | 'T' | 't' -> Some 'A'
    | 'A' | 'a' -> Some 'U'
    | _ -> None
  dna
  |> Seq.map trans
  |> Seq.choose id
  |> Seq.map string
  |> String.concat ""
