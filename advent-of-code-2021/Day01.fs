module advent_of_code_2021.Day01

open System.IO


let parse (strs: string seq) = strs |> Seq.map int |> Seq.toList

let zipshift left right =
    List.zip (left |> List.take (left.Length - 1)) (right |> List.skip 1)

let part1_old (numbers: int list) =
    zipshift numbers numbers
    |> Seq.filter (fun (a, b) -> b > a)
    |> Seq.length

let part2_old (numbers: int list) =
    zipshift (numbers |> List.take (numbers.Length - 1)) (zipshift numbers numbers)
    |> List.map (fun (a, (b, c)) -> a + b + c)
    |> part1_old

let part1 numbers =
    numbers
    |> Seq.pairwise
    |> Seq.filter (fun (a, b) -> b > a)
    |> Seq.length
    
let part2 numbers = 
    numbers
    |> Seq.windowed 3
    |> Seq.map (fun i -> i |> Seq.sum)
    |> part1

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
