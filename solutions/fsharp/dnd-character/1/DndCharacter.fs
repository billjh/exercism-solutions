module DndCharacter

open System

let modifier x =
    (float (x - 10) / 2.0)
    |> floor
    |> int

let ability() = 
    let getDice() = Random().Next 7
    {1 .. 4} 
    |> Seq.map (fun _ -> getDice())
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

type DndCharacter() =
    member val Strength = ability() with get
    member val Dexterity = ability() with get
    member val Constitution = ability() with get
    member val Intelligence = ability() with get
    member val Wisdom = ability() with get
    member val Charisma = ability() with get
    member this.Hitpoints with get() = 10 + modifier(this.Constitution)

