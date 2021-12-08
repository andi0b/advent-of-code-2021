module advent_of_code_2021.Day04

open System
open System.IO
open Swensen.Unquote
open Xunit

type Board = int [,]
type Numbers = int list

let NL = Environment.NewLine

module Board =
    let size = 5

    let fromFlat (numbers: int array) =
        Array2D.init size size (fun x y -> numbers.[x * size + y])

    let col i (board: Board) = board.[*, i]

    let row i (board: Board) = board.[i, *]

    let rows board =
        [| 0 .. size - 1 |]
        |> Array.map (fun i -> row i board)

    let cols board =
        [| 0 .. size - 1 |]
        |> Array.map (fun i -> col i board)

    let slices board = Array.concat [ cols board; rows board ]

    let isBingo numbers (board: Board) =
        let sliceIsBingo =
            Array.forall (fun x -> numbers |> Set.contains x)

        slices board |> Array.exists sliceIsBingo

    let score (numbers: Numbers) (board: Board) =
        let unmarkedNumbers =
            board
            |> rows
            |> Seq.collect id
            |> Seq.filter (fun x -> numbers |> List.contains x |> not)

        (unmarkedNumbers |> Seq.sum)
        * (numbers |> List.last)

    let fromString (input: string) =
        let numbersStr =
            input.Split([| NL; " " |], StringSplitOptions.RemoveEmptyEntries)

        numbersStr |> Array.map int |> fromFlat


type Puzzle =
    { numbers: Numbers
      boards: Board list }

module Puzzle =
    let parse (input: string) =

        let firstLine, remainingInput =
            let result = input.Split(NL, 2)
            (result.[0], result.[1])

        let numbers =
            firstLine.Split(',')
            |> Array.map int
            |> List.ofArray

        let boards =
            remainingInput.Split(NL + NL)
            |> Array.map Board.fromString
            |> List.ofArray

        { numbers = numbers; boards = boards }


let solve winnerSelector puzzle =

    let allWinners boards numbers =
        let numbersSet = numbers |> Set.ofSeq

        let bingoFilter board =
            if (board |> Board.isBingo numbersSet) then
                Some(numbers, board)
            else
                None

        boards |> List.choose bingoFilter

    let rounds =
        [ 0 .. puzzle.numbers.Length - 1 ]
        |> List.map (fun len -> puzzle.numbers.[0..len])


    let winnerReducer (remainingBoards: Board list, winners: (Numbers * Board) list) (round: Numbers) =
        let roundWinners = allWinners remainingBoards round

        (remainingBoards
         |> List.except (roundWinners |> List.map snd),
         winners @ roundWinners)

    let winners =
        rounds
        |> List.fold winnerReducer (puzzle.boards, [])
        |> snd

    let winningNumbers, winningBoard = winnerSelector winners

    winningBoard |> Board.score winningNumbers

let part1 = solve List.head

let part2 = solve List.last

let run () =
    let puzzle =
        File.ReadAllText("Day04.txt") |> Puzzle.parse

    $"{part1 puzzle} {part2 puzzle}"

module tests =
    let input =
        """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

    let puzzle = Puzzle.parse input

    [<Fact>]
    let parseTests () =
        test <@ puzzle.numbers.[..2] = [ 7; 4; 9 ] @>
        test <@ puzzle.boards.Length = 3 @>
        puzzle

    [<Fact>]
    let boardTests () =
        let board1 = puzzle.boards.[0]
        let board3 = puzzle.boards.[2]
        test <@ board1 |> Board.row 0 = [| 22; 13; 17; 11; 0 |] @>
        test <@ board1 |> Board.col 0 = [| 22; 8; 21; 6; 1 |] @>

        let first11 = puzzle.numbers.[0..10] |> Set.ofSeq
        let first12 = puzzle.numbers.[0..11] |> Set.ofSeq
        test <@ board1 |> Board.isBingo first11 = false @>
        test <@ board3 |> Board.isBingo first11 = false @>
        test <@ board1 |> Board.isBingo first12 = false @>
        test <@ board3 |> Board.isBingo first12 = true @>

    [<Fact>]
    let part1 () = test <@ part1 puzzle = 4512 @>

    [<Fact>]
    let part2 () = test <@ part2 puzzle = 1924 @>
