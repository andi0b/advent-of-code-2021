module advent_of_code_2021.Day06

open System.IO

let nextState =
    Seq.collect
        (function
        | 0 -> [ 6; 8 ]
        | i -> [ i - 1 ])

let part1 (fishAges: int seq) =
    [ 1 .. 80 ]
    |> Seq.fold (fun acc _ -> nextState acc) fishAges
    |> Seq.length

type FishGroup = { age: int; count: int64 }
module FishGroup =
    let totalFishCount (groups: FishGroup seq) = groups |> Seq.sumBy (fun g -> g.count)

    let fromAgeList (fishAges: int seq) =
        fishAges
        |> Seq.groupBy id
        |> Seq.map (fun (age, fish) -> { age = age; count = Seq.length fish })
        
    let nextGroupState =
        Seq.collect
            (function
            | g when g.age = 0 ->
                [ { g with age = 6 }
                  { g with age = 8 } ]
            | g -> [ { g with age = g.age - 1 } ])
        >> Seq.groupBy (fun g -> g.age)
        >> Seq.map
            (fun (age, g) ->
                { age = age
                  count = (totalFishCount g) })

let part2 (fishAges: int seq) =
    let fishGroups = FishGroup.fromAgeList fishAges

    [ 1 .. 256 ]
    |> Seq.fold (fun acc _ -> FishGroup.nextGroupState acc) fishGroups
    |> FishGroup.totalFishCount

let test =
    let input = [ 3; 4; 3; 1; 2 ]

    let part1 =
        let d1 = nextState input
        let d2 = nextState d1
        let d3 = nextState d2
        let d4 = nextState d3

        let compare s1 s2 =
            Seq.compareWith Operators.compare (Seq.sort s1) (Seq.sort s2) = 0

        assert (compare d1 [ 2; 3; 2; 0; 1 ])
        assert (compare d2 [ 1; 2; 1; 6; 0; 8 ])
        assert (compare d3 [ 0; 1; 0; 5; 6; 7; 8 ])
        assert (compare d4 [ 6; 0; 6; 4; 5; 6; 7; 8; 8 ])

        assert (part1 input = 5934)

    part1

    assert (part2 input = 26984457539L)



let result =
    test

    let fishAges =
        File.ReadAllLines("Day06.txt").[0].Split(",")
        |> Array.map int

    $"{part1 fishAges} {part2 fishAges}"
