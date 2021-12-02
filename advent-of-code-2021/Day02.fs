﻿module advent_of_code_2021.Day02

open System.IO

let parse (str: string) =
    let parts = str.Split(" ")
    let distance = parts.[1] |> int

    match parts.[0] with
    | "forward" -> (distance, 0)
    | "down" -> (0, distance)
    | "up" -> (0, -distance)
    | _ -> failwith $"Can't parse {parts.[0]}"

let calcAnswer (a, b) = a * b

let part1 instructions =
    instructions
    |> Seq.reduce (fun (a1, b1) (a2, b2) -> (a1 + a2, b1 + b2))
    |> calcAnswer

type Submarine = { position: int * int; aim: int }

let part2 instructions =

    let calcNextPosition (sub: Submarine) (forward, aimChange) =
        let (horizontal, depth) = sub.position

        { position = (horizontal + forward, depth + sub.aim * forward)
          aim = sub.aim + aimChange }

    (Seq.fold calcNextPosition { position = (0, 0); aim = 0 } instructions)
        .position
    |> calcAnswer

let test =
    let parsedInstructions =
        [ "forward 5"
          "down 5"
          "forward 8"
          "up 3"
          "down 8"
          "forward 2" ]
        |> List.map parse

    assert (part1 parsedInstructions = 150)
    assert (part2 parsedInstructions = 900)


let result =
    test

    let parsedInstructions =
        File.ReadAllLines("Day02.txt") |> Seq.map parse

    $"{part1 parsedInstructions} {part2 parsedInstructions}"
