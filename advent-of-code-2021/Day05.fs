module advent_of_code_2021.Day05

open System
open System.IO
open System.Linq
open System.Text.RegularExpressions

type Line =
    { x1: int
      y1: int
      x2: int
      y2: int }
    override m.ToString() = $"{m.x1},{m.y1} -> {m.x2},{m.y2}"

[<Struct>]
type Point = { x: int; y: int }


module Line =
    let vector line = (line.x2 - line.x1, line.y2 - line.y1)
    let isHorizontal line = line.x1 = line.x2
    let isVertical line = line.y1 = line.y2

    let allPoints line =
        let vecX, vecY = vector line

        let divisor =
            [ vecX; vecY ]
            |> Seq.filter (fun x -> x <> 0)
            |> Seq.min
            |> (Math.Abs >> int)

        let normX, normY = (vecX / divisor, vecY / divisor)

        { 0 .. divisor }
        |> Seq.map
            (fun i ->
                { x = line.x1 + normX * i
                  y = line.y1 + normY * i })

    let parse str =
        match Regex.Match(str, "(\d+),(\d+) -> (\d+),(\d+)") with
        | m when m.Success ->
            let g = m.Groups

            { x1 = g.[1].Value |> int
              y1 = g.[2].Value |> int
              x2 = g.[3].Value |> int
              y2 = g.[4].Value |> int }
        | _ -> failwith ($"can't parse: {str}")

let allPairsSelf =
    Seq.mapi (fun i v -> (i, v))
    >> (fun x -> Seq.allPairs x x)
    >> Seq.filter (fun ((i1, _), (i2, _)) -> i1 <> i2)
    >> Seq.map (fun ((_, v1), (_, v2)) -> (v1, v2))

let numberOfIntersections lines =
    lines
    |> allPairsSelf
    |> Seq.map (fun (l1, l2) -> (Line.allPoints l1, Line.allPoints l2))
    |> Seq.collect Enumerable.Intersect
    |> Seq.distinct
    |> Seq.length

let part1 lines =
    lines
    |> Seq.filter (fun l -> Line.isHorizontal l || Line.isVertical l)
    |> numberOfIntersections

let part2 = numberOfIntersections

let test =
    let input =
        [ "0,9 -> 5,9"
          "8,0 -> 0,8"
          "9,4 -> 3,4"
          "2,2 -> 2,1"
          "7,0 -> 7,4"
          "6,4 -> 2,0"
          "0,9 -> 2,9"
          "3,4 -> 1,4"
          "0,0 -> 8,8"
          "5,5 -> 8,2" ]

    let lines = input |> List.map Line.parse

    assert (part1 lines = 5)
    assert (part2 lines = 12)

let result =
    test

    let lines =
        File.ReadAllLines("Day05.txt")
        |> Array.map Line.parse

    $"{part1 lines} {part2 lines}"
