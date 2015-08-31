module Katas.BoardGameTests

open Katas.BoardGame
open NUnit.Framework

let directionToString d =
    match d with
    | North -> "North"
    | East -> "East"
    | South -> "South"
    | West -> "West"

[<Test>]
let TurnRight() =
    [(North, East);
    (East, South);
    (South, West);
    (West, North)]
    |> List.map (fun t -> 
        let gotDirection = turnRight (fst t)
        Assert.AreEqual(gotDirection, snd t, "Expected: " + directionToString(snd t) + ", got: " + directionToString(gotDirection)))
    |> ignore

[<Test>]
let TurnLeft() =
    [(North, West);
    (East, North);
    (South, East);
    (West, South)]
    |> List.map (fun t -> 
        let gotDirection = turnLeft (fst t)
        Assert.AreEqual(gotDirection, snd t, "Expected: " + directionToString(snd t) + ", got: " + directionToString(gotDirection)))
    |> ignore

[<Test>]
let GetCurrentPosition() =
    Assert.AreEqual(currentPlayer.CurrentPosition.X, 0)
    Assert.AreEqual(currentPlayer.CurrentPosition.Y, 0)
