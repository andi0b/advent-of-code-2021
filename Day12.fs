module advent_of_code_2021.Day12

open System.IO
open Swensen.Unquote
open Xunit

type CaveMap = Map<string, string list>

let createCaveMap input : CaveMap =

    let parse (x: string) =
        let parts = x.Split('-')
        parts.[0], parts.[1]

    input
    |> Seq.map parse
    |> Seq.collect (fun x -> [ x; (snd x, fst x) ])
    |> Seq.groupBy fst
    |> Seq.map (fun (key, values) -> (key, values |> Seq.map snd |> Seq.toList))
    |> Map.ofSeq

let isBigCave: string -> bool = Seq.forall System.Char.IsUpper

let rec walkCaves startingPoint visitedCaves (canTravel: string list -> string -> bool) (caveMap: CaveMap) =

    let visitedCaves = startingPoint :: visitedCaves

    let connectedCaves = caveMap |> Map.find startingPoint

    let availableConnectedCaves =
        connectedCaves
        |> List.filter (canTravel visitedCaves)

    let paths =
        availableConnectedCaves
        |> List.collect
            (function
            | "end" -> [ "end" :: visitedCaves ]
            | x -> walkCaves x visitedCaves canTravel caveMap)

    match availableConnectedCaves with
    | [] -> []
    | _ -> paths


let part1 input =

    let canTravel visitedCaves cave =
        isBigCave cave
        || not (visitedCaves |> List.contains cave)

    let paths =
        walkCaves "start" [] canTravel (input |> createCaveMap)

    paths |> List.length


let part2 input =

    let caveMap = input |> createCaveMap

    let smallCaves =
        caveMap.Keys
        |> Seq.filter (fun x -> not <| isBigCave x)
        |> Seq.except [ "start"; "end" ]
        |> Seq.toList

    let paths doubleCave =

        let canTravel visitedCaves cave =
            isBigCave cave
            || (cave = doubleCave
                && visitedCaves
                   |> Seq.filter (fun x -> x = doubleCave)
                   |> Seq.length = 1)
            || not (visitedCaves |> List.contains cave)

        walkCaves "start" [] canTravel caveMap

    let allPaths =
        smallCaves |> List.collect paths |> List.distinct

    allPaths |> List.length

let run () =
    let input = File.ReadAllLines("inputs/Day12.txt")
    $"{part1 input} {part2 input}"


module test =
    let demoInput =
        [ "start-A"
          "start-b"
          "A-c"
          "A-b"
          "b-d"
          "A-end"
          "b-end" ]

    [<Fact>]
    let part1 () = test <@ part1 demoInput = 10 @>

    [<Fact>]
    let part2 () = test <@ part2 demoInput = 36 @>

    [<Fact>]
    let caveMap () =

        let input = [ "start-bb"; "a-bb" ]

        let expected =
            [ "a", [ "bb" ]
              "bb", [ "start"; "a" ]
              "start", [ "bb" ] ]
            |> Map.ofList

        test <@ createCaveMap input = expected @>
