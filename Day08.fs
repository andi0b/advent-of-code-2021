module advent_of_code_2021.Day08

open System
open System.IO
open Swensen.Unquote
open Xunit

module Parser =
    let parseBinary (input: string) =
        input.ToCharArray()
        |> Array.map (fun c -> (c |> int) - ('a' |> int))
        |> Array.fold (fun state shift -> (1 <<< shift) ||| state) 0

    let parsePart (str: string) =
        str.Split(" ")
        |> Array.map parseBinary
        |> Array.toList

    let parse (line: string) =
        let parts = line.Split(" | ")

        let patterns = parsePart parts.[0]
        let outputs = parsePart parts.[1]

        (patterns, outputs)

let segmentCount value =
    value
    |> uint32
    |> System.Numerics.BitOperations.PopCount

let part1 (lines: string list) =
    lines
    |> Seq.collect (Parser.parse >> snd >> (List.map segmentCount))
    |> Seq.filter (fun len -> len = 2 || len = 3 || len = 4 || len = 7)
    |> Seq.length

let getDisplayValue (patterns: int list, outputs: int list) =

    let one, four =
        let findPattern c =
            List.find (fun x -> segmentCount x = c) patterns

        (findPattern 2, findPattern 4)

    let decode p =
        match (segmentCount p, segmentCount (p &&& one), segmentCount (p &&& four)) with
        | (2, _, _) -> 1
        | (3, _, _) -> 7
        | (4, _, _) -> 4
        | (5, 2, _) -> 3
        | (5, _, 2) -> 2
        | (5, _, 3) -> 5
        | (6, _, 4) -> 9
        | (6, 2, _) -> 0
        | (6, 1, _) -> 6
        | (7, _, _) -> 8
        | _ -> failwith ("unknown segment value")

    let resolvedMappings =
        patterns
        |> List.map (fun x -> (x, decode x))
        |> Map.ofList

    outputs
    |> List.rev
    |> List.mapi (fun i x -> resolvedMappings.[x] * (pown 10 i))
    |> List.sum

let part2 (lines: string list) =
    lines
    |> List.map Parser.parse
    |> List.map getDisplayValue
    |> List.sum

let run () =
    let input =
        File.ReadAllLines("inputs/Day08.txt") |> List.ofArray

    $"{part1 input} {part2 input}"

module test =
    let input =
        [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
          "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
          "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
          "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
          "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
          "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
          "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
          "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
          "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
          "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" ]

    [<Fact>]
    let part1 () = test <@ part1 input = 26 @>

    [<Fact>]
    let part2 () = test <@ part2 input = 61229 @>

    [<Fact>]
    let parseBitwise () =
        test <@ Parser.parseBinary "a" = 1 @>
        test <@ Parser.parseBinary "b" = 2 @>
        test <@ Parser.parseBinary "ab" = 3 @>
        test <@ Parser.parseBinary "bc" = 6 @>

    [<Fact>]
    let solve2 () =

        let parsed =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
            + " | cdfeb fcadb cdfeb cdbaf"
            |> Parser.parse

        test <@ getDisplayValue parsed = 5353 @>
