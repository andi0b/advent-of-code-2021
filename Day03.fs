module advent_of_code_2021.Day03

open System
open System.IO
open Xunit
open Swensen.Unquote
open Xunit.Sdk



let calcEpsilon (report: string list) =
    let len = report.[0].Length

    let countOnesAt pos =
        report
        |> Seq.filter (fun x -> x.[pos] = '1')
        |> Seq.length

    let numAt pos =
        let ones = countOnesAt pos

        if ones > (report.Length / 2) then
            1 <<< (len - 1 - pos)
        else
            0

    [ 0 .. len - 1 ]
    |> Seq.fold (fun acc next -> (acc ||| numAt next)) 0

let calcGamma (epsilon: int) (numberLength: int) =
    let mask = ((2.0 ** numberLength) |> int) - 1
    epsilon ^^^ mask

let part1 report =
    let epsilon = calcEpsilon report
    let gamma = calcGamma epsilon report.[0].Length
    epsilon * gamma


let reduce chooser def reports =

    let rec reduceInner (chooser: int list -> int) (def: char) currentPosition (reports: string list) =

        let getCurrentPosition (x: string) = x.[currentPosition]

        let groups =
            reports
            |> List.groupBy getCurrentPosition
            |> List.map (fun (_, list) -> (list.Length, list))

        let chosenCount = groups |> List.map fst |> chooser

        let remaining =
            groups
            |> List.filter (fun (count, _) -> count = chosenCount)
            |> List.map snd
            |> List.tryExactlyOne
            |> Option.defaultWith
                (fun () ->
                    reports
                    |> List.filter (fun x -> (getCurrentPosition x) = def))

        match remaining with
        | [ _ ] -> remaining
        | _ -> reduceInner chooser def (currentPosition + 1) remaining

    reduceInner chooser def 0 reports
    |> List.exactlyOne

let part2 (report: string list) =

    let fromBinary x = Convert.ToInt32(x, 2)

    let oxygenRating = reduce List.max '1' report |> fromBinary
    let co2Rating = reduce List.min '0' report |> fromBinary

    oxygenRating * co2Rating


module test =
    let report =
        [ "00100"
          "11110"
          "10110"
          "10111"
          "10101"
          "01111"
          "00111"
          "11100"
          "10000"
          "11001"
          "00010"
          "01010" ]


    [<Fact>]
    let part1 () = test <@ part1 report = 198 @>


    [<Fact>]
    let part2 () = test <@ part2 report = 230 @>

let result =
    let instructions =
        File.ReadAllLines("inputs/Day03.txt")
        |> Seq.toList

    $"{part1 instructions} {part2 instructions}"
