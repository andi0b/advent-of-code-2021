module advent_of_code_2021.Day09

open System.IO
open Swensen.Unquote
open Xunit

type HeightMap = int [,]

module HeightMap =
    let parse (lines: string array) : HeightMap =
        let cols = lines.[0].Length
        let rows = lines.Length

        Array2D.init cols rows (fun x y -> lines.[y].[x] |> string |> int)

    let getAdjacent (x, y) (heightMap: HeightMap) =
        let inBounds (x, y) =
            x >= 0
            && y >= 0
            && x < Array2D.length1 heightMap
            && y < Array2D.length2 heightMap

        let getWithOffset (offX, offY) =
            match (x + offX, y + offY) with
            | px, py when inBounds (px, py) -> Some((px, py), heightMap.[px, py])
            | _ -> None

        [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
        |> List.choose getWithOffset

    let isLowPoint (x, y) (heightMap: HeightMap) =
        heightMap
        |> getAdjacent (x, y)
        |> List.forall (fun (_, adjVal) -> adjVal > heightMap.[x, y])

    let lowPoints (heightMap: HeightMap) =
        heightMap
        |> Array2D.mapi
            (fun x y value ->
                match (heightMap |> isLowPoint (x, y)) with
                | true -> Some((x, y), value)
                | false -> None)
        |> Seq.cast<((int * int) * int) option>
        |> Seq.choose id
        |> Seq.toList

    let calculateBasinSize startPoint (heightMap: HeightMap) =
        let findHigherAdjacent (pos, baseValue) =
            heightMap
            |> getAdjacent pos
            |> List.filter (fun (_, value) -> value > baseValue && value < 9)

        let rec findBasinPoints basinPoints =
            match basinPoints
                  |> List.collect findHigherAdjacent
                  |> List.distinct
                  |> List.except basinPoints with
            | [] -> basinPoints
            | higherPoints -> findBasinPoints (basinPoints @ higherPoints)

        findBasinPoints [ startPoint ] |> List.length

let part1 input =
    input
    |> HeightMap.parse
    |> HeightMap.lowPoints
    |> List.map snd
    |> List.sumBy ((+) 1)

let part2 input =
    let parsed = HeightMap.parse input

    parsed
    |> HeightMap.lowPoints
    |> List.map (fun lowPoint -> parsed |> HeightMap.calculateBasinSize lowPoint)
    |> Seq.sortByDescending id
    |> Seq.take 3
    |> Seq.reduce (*)

let run () =
    let input = File.ReadAllLines("inputs/Day09.txt")

    $"{part1 input} {part2 input}"

module test =
    let testData =
        [| "2199943210"
           "3987894921"
           "9856789892"
           "8767896789"
           "9899965678" |]

    let parsed = testData |> HeightMap.parse

    [<Fact>]
    let part1 () = test <@ part1 testData = 15 @>

    [<Fact>]
    let part2 () = test <@ part2 testData = 1134 @>

    [<Fact>]
    let parse () =
        test <@ parsed.[0, 0] = 2 @>
        test <@ parsed.[9, 0] = 0 @>
        test <@ parsed.[9, 4] = 8 @>

    [<Fact>]
    let findLowPoints () =
        let lowPoints =
            parsed |> HeightMap.lowPoints |> List.map snd

        test <@ (lowPoints |> List.sort) = [ 0; 1; 5; 5 ] @>

    [<Fact>]
    let basinSize () =
        test <@ parsed |> HeightMap.calculateBasinSize ((1, 0), 1) = 3 @>
        test <@ parsed |> HeightMap.calculateBasinSize ((2, 2), 5) = 14 @>
