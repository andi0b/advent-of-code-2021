module advent_of_code_2021.Day01

open System.IO

let part1 =
    Seq.pairwise
    >> Seq.filter (fun (a, b) -> b > a)
    >> Seq.length

let part2 =
    Seq.windowed 3 >> Seq.map Seq.sum >> part1

let parse (strings: string seq) = strings |> Seq.map int |> Seq.toList

let test =
    let testData =
        parse [ "199"
                "200"
                "208"
                "210"
                "200"
                "207"
                "240"
                "269"
                "260"
                "263" ]

    assert (part1 testData = 7)
    assert (part2 testData = 5)

let result =
    test
    let parsed = File.ReadAllLines("Day01.txt") |> parse
    $"{part1 parsed} {part2 parsed}"
