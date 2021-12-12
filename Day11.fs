module advent_of_code_2021.Day11

open System.IO
open System.Reflection.Emit
open Swensen.Unquote
open Xunit

type OctopusMap = int [,]

module OctopusMap =
    let parse (lines: string list) : OctopusMap =
        let cols = lines.[0].Length
        let rows = lines.Length

        Array2D.init cols rows (fun x y -> lines.[y].[x] |> string |> int)

    let toString (map: OctopusMap) =
        seq {
            for y in 0 .. ((map |> Array2D.length1) - 1) do
                yield
                    new string (
                        map.[*, y]
                        |> Array.map (fun x -> (('0' |> int) + x) |> char)
                    )
        }
        |> Seq.toList

    let find predicate (map: OctopusMap) =
        seq {
            for x in 0 .. ((map |> Array2D.length1) - 1) do
                for y in 0 .. ((map |> Array2D.length2) - 1) do
                    if predicate map.[x, y] then
                        yield (x, y)
        }

    let findFlashers = find (fun x -> x > 9)

    let adjacentPositions (x, y) (map: OctopusMap) =
        let inBounds (x, y) =
            x >= 0
            && y >= 0
            && x < Array2D.length1 map
            && y < Array2D.length2 map

        let getWithOffset (offX, offY) =
            match (x + offX, y + offY) with
            | px, py when inBounds (px, py) -> Some(px, py)
            | _ -> None

        [ -1 .. 1 ]
        |> List.allPairs [ -1 .. 1 ]
        |> List.except [ (0, 0) ]
        |> List.choose getWithOffset

    let step (map: OctopusMap) =

        let newMap = map |> Array2D.map ((+) 1)

        let rec getFlashCount alreadyFlashed flashCount =

            let newFlashers =
                newMap
                |> findFlashers
                |> Seq.except alreadyFlashed
                |> List.ofSeq

            newFlashers
            |> List.collect (fun x -> adjacentPositions x map)
            |> List.iter (fun (x, y) -> newMap.[x, y] <- (newMap.[x, y] + 1))

            match newFlashers with
            | [] ->
                (newMap
                 |> Array2D.map (fun i -> if i <= 9 then i else 0),
                 flashCount)
            | _ -> getFlashCount (alreadyFlashed @ newFlashers) (flashCount + newFlashers.Length)


        getFlashCount [] 0


let part1 input =
    let map = input |> OctopusMap.parse

    let folder (state, flashCount) _ =
        let newMap, newFlashes = state |> OctopusMap.step
        (newMap, flashCount + newFlashes)

    [ 1 .. 100 ] |> List.fold folder (map, 0) |> snd


let part2 input =
    let mutable map = input |> OctopusMap.parse

    let isZero map =
        map
        |> Seq.cast<int>
        |> Seq.forall (fun x -> x = 0)

    let rec solve state i =
        let nextState, _ = state |> OctopusMap.step

        if isZero nextState then
            i + 1
        else
            solve nextState i + 1

    solve map 0

let run () =
    let input =
        File.ReadAllLines("inputs/Day11.txt")
        |> List.ofArray

    $"{part1 input} {part2 input}"

module test =
    let testData =
        [ "5483143223"
          "2745854711"
          "5264556173"
          "6141336146"
          "6357385478"
          "4167524645"
          "2176841721"
          "6882881134"
          "4846848554"
          "5283751526" ]

    let step1 =
        [ "6594254334"
          "3856965822"
          "6375667284"
          "7252447257"
          "7468496589"
          "5278635756"
          "3287952832"
          "7993992245"
          "5957959665"
          "6394862637" ]

    let step2 =
        [ "8807476555"
          "5089087054"
          "8597889608"
          "8485769600"
          "8700908800"
          "6600088989"
          "6800005943"
          "0000007456"
          "9000000876"
          "8700006848" ]


    let parsed = testData |> OctopusMap.parse

    [<Fact>]
    let ```parse and tostring`` () =
        test
            <@ (testData
                |> OctopusMap.parse
                |> OctopusMap.toString) = testData @>

    [<Fact>]
    let part1 () = test <@ part1 testData = 1656 @>

    [<Fact>]
    let part2 () = test <@ part2 testData = 195 @>

    [<Fact>]
    let ``simpleExample`` () =
        let initial =
            [ "11111"
              "19991"
              "19191"
              "19991"
              "11111" ]

        let step1 =
            [ "34543"
              "40004"
              "50005"
              "40004"
              "34543" ]

        let step2 =
            [ "45654"
              "51115"
              "61116"
              "51115"
              "45654" ]

        let after1 =
            initial
            |> OctopusMap.parse
            |> OctopusMap.step
            |> fst

        test <@ (after1 |> OctopusMap.toString) = step1 @>


        let after2 = after1 |> OctopusMap.step |> fst

        test <@ (after2 |> OctopusMap.toString) = step2 @>



    [<Fact>]
    let ``after step 1`` () =
        test
            <@ (parsed
                |> OctopusMap.step
                |> fst
                |> OctopusMap.toString) = step1 @>

    [<Fact>]
    let ``after step 2`` () =
        test
            <@ (parsed
                |> OctopusMap.step
                |> fst
                |> OctopusMap.step
                |> fst
                |> OctopusMap.toString) = step2 @>
