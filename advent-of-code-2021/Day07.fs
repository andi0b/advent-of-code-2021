module advent_of_code_2021.Day07

open System.IO

let parse (str: string) =
    str.Split(',') |> Seq.map int |> List.ofSeq

let solve (calcFuelConsumption: int -> int -> int) (positions: int list) =

    let calcFuelConsumptions pos =
        positions
        |> Seq.map (calcFuelConsumption pos)
        |> Seq.sum

    let results =
        [ Seq.min positions .. Seq.max positions ]
        |> List.map calcFuelConsumptions

    results |> List.min

let part1 =
    solve (fun pos1 pos2 -> pos1 - pos2 |> abs)

let part2 =
    let factorialAdd n =
        match n with
        | 0 -> 0
        | _ -> [ 1 .. abs n ] |> List.reduce (+)
        
    // muuuuch faster :)
    let triangleNumber n = ((pown n 2) + n) / 2

    solve (fun pos1 pos2 -> pos1 - pos2 |> abs |> triangleNumber)

let test =
    let input = "16,1,2,0,4,2,7,1,2,14" |> parse

    assert (part1 input = 37)
    assert (part2 input = 168)

    ()

let result =
    test

    let input =
        File.ReadAllLines("Day07.txt").[0] |> parse

    $"{part1 input} {part2 input}"
