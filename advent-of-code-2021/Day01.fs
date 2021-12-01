module advent_of_code_2021.Day01

open System.IO

let part1 numbers =
    numbers
    |> Seq.pairwise
    |> Seq.filter (fun (a, b) -> b > a)
    |> Seq.length

let part2 numbers = 
    numbers
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> part1

let parse (strs: string seq) = strs |> Seq.map int |> Seq.toList

let test =
    let parsed =
        [ "199"
          "200"
          "208"
          "210"
          "200"
          "207"
          "240"
          "269"
          "260"
          "263" ]
        |> parse

    assert (parsed |> part1 = 7)
    assert (parsed |> part2 = 5)

let result =
    test
    let parsed = File.ReadAllLines("Day01.txt") |> parse
    $"{part1 parsed}/{part2 parsed}"
