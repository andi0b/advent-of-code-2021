module advent_of_code_2021.Day08

open System
open System.IO
open Swensen.Unquote
open Xunit

// 0 = abc_efg 6
// 1 = __c__f_ 2
// 2 = a_cde_g 5
// 3 = a_cd_fg 5
// 4 = _bcd_f_ 4
// 5 = ab_d_fg 5
// 6 = ab_defg 6
// 7 = a_c__f_ 3
// 8 = abcdefg 7
// 9 = abcd_fg 6

// 2 on => 1
// 3 on => 7
// 4 on => 4
// 5 on => 2 3 5
// 6 on => 0 6 9
// 7 on => 8


let parse (line: string) =
    let parts = line.Split(" | ")
    let patterns = parts.[0].Split(" ")
    let outputs = parts.[1].Split(" ")

    (patterns, outputs)

let parseBinary (input: string) =
    input.ToCharArray()
    |> Array.map (fun c -> (c |> int) - ('a' |> int))
    |> Array.fold (fun state shift -> (1 <<< shift) ||| state) 0

let part1 (lines: string list) =
    lines
    |> Seq.collect (parse >> snd >> (Array.map String.length))
    |> Seq.filter (fun len -> len = 2 || len = 3 || len = 4 || len = 7)
    |> Seq.length

let segmentCount value =
    value
    |> uint32
    |> System.Numerics.BitOperations.PopCount

let firstPass value =
    match (segmentCount value) with
    | 2 -> Some 1
    | 3 -> Some 7
    | 4 -> Some 4
    | 7 -> Some 8
    | _ -> None

let secondPass value (resolvedMappings: Map<int, int>) =
    let one = resolvedMappings.[1]
    let four = resolvedMappings.[4]

    let solve5 =
        if ((value &&& one) = one) then
            3
        else
            match ((value &&& four) |> segmentCount) with
            | 2 -> 2
            | 3 -> 5
            | i -> failwith ($"invalid 7 segment number, remaining segments after &&& four = {i}")

    let solve6 =
        if ((value &&& four) = four) then 9
        elif ((value &&& one) = one) then 0
        else 6

    match (segmentCount value) with
    | 5 -> solve5
    | 6 -> solve6
    | _ -> failwith ("invalid 7 segment number")

let solve2 (patternsRaw: string [], outputsRaw: string []) =

    let toBinary = Seq.map parseBinary >> Seq.toList

    let patterns = patternsRaw |> toBinary
    let outputs = outputsRaw |> toBinary

    let firstResolvedMappings =
        patterns
        |> List.choose (fun p -> firstPass p |> Option.map (fun r -> (r, p)))
        |> Map.ofSeq

    let secondResolvedMappings =
        patterns
        |> List.except firstResolvedMappings.Values
        |> List.map (fun p -> (secondPass p firstResolvedMappings, p))

    let resolvedMappings =
        secondResolvedMappings
        @ (firstResolvedMappings |> Map.toList)
        |> List.map (fun (key, value) -> (value, key))
        |> Map.ofList

    outputs
    |> List.map (fun x -> resolvedMappings.[x])
    |> List.rev
    |> List.mapi (fun i x -> (pown 10 i) * x)
    |> List.reduce (+)


let part2 (lines: string list) =
    lines
    |> List.map parse
    |> List.map solve2
    |> List.reduce (+)


let run () =
    let input =
        File.ReadAllLines("Day08.txt") |> List.ofArray

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
        test <@ parseBinary "a" = 1 @>
        test <@ parseBinary "b" = 2 @>
        test <@ parseBinary "ab" = 3 @>
        test <@ parseBinary "bc" = 6 @>

    [<Fact>]
    let solve2 () =

        let patterns =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
                .Split(' ')

        let outputs = "cdfeb fcadb cdfeb cdbaf".Split(' ')

        test <@ solve2 (patterns, outputs) = 5353 @>
