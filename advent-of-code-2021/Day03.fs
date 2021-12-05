module advent_of_code_2021.Day03

open System
open System.IO



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

let test =
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

    assert (part1 report = 198)
//assert (part2 parsedInstructions = 900)


let result =
    test

    let instructions =
        File.ReadAllLines("Day03.txt") |> Seq.toList

    $"{part1 instructions} {part1 instructions}"
