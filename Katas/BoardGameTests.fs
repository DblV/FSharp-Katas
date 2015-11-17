module Katas.BoardGameTests

open Katas.BoardGame
open NUnit.Framework

let directionToString d =
    match d with
    | North -> "North"
    | East -> "East"
    | South -> "South"
    | West -> "West"

let describeStates state expectedState = 
    sprintf "Expected %A but got %A" state expectedState

let initialPlayerState = {
    CurrentPosition = { X=0; Y=0 }
    CurrentDirection = North
    }
   
let boardWidth = 5

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
let TurnPlayerRight() = 
    [(North, East);
    (East, South);
    (South, West);
    (West, North)]
    |> List.map (fun t -> 
        let playerState = turnPlayerRight { CurrentPosition={X=0;Y=0}; CurrentDirection=(fst t)}
        Assert.AreEqual(playerState.CurrentDirection, snd t, "Expected: " + directionToString(snd t) + ", got: " + directionToString(playerState.CurrentDirection)))
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
let TurnPlayerLeft() = 
    [(North, West);
    (East, North);
    (South, East);
    (West, South)]
    |> List.map (fun t -> 
        let playerState = turnPlayerLeft { CurrentPosition={X=0;Y=0}; CurrentDirection=(fst t)}
        Assert.AreEqual(playerState.CurrentDirection, snd t, "Expected: " + directionToString(snd t) + ", got: " + directionToString(playerState.CurrentDirection)))
    |> ignore

[<Test>]
let MovePlayer() = 
    let newPlayerState = movePlayer boardWidth initialPlayerState
    let expectedState = { initialPlayerState with CurrentPosition = { X=0; Y=1 }}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerMMMMM() = 
    let newPlayerState = applyMoves boardWidth ['M';'M';'M';'M';'M';'M'] initialPlayerState
    let expectedState = { CurrentDirection = North; CurrentPosition = {X=0; Y=5}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerRMMMM() = 
    let newPlayerState = applyMoves boardWidth ['R';'M';'M';'M';'M';'M';'M'] initialPlayerState
    let expectedState = { CurrentDirection = East; CurrentPosition = {X=5; Y=0}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerLRLRM() = 
    let newPlayerState = applyMoves boardWidth ['L';'R';'L';'R';'M'] initialPlayerState
    let expectedState = { CurrentDirection = North; CurrentPosition = {X=0; Y=1}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerMRMLM() = 
    let newPlayerState = applyMoves boardWidth ['M';'R';'M';'L';'M'] initialPlayerState
    let expectedState = { CurrentDirection = North; CurrentPosition = {X=1; Y=2}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerRMMLM() = 
    let newPlayerState = applyMoves boardWidth ['R';'M';'M';'L';'M'] initialPlayerState
    let expectedState = { CurrentDirection = North; CurrentPosition = {X=2; Y=1}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerRRRRRR() = 
    let newPlayerState = applyMoves boardWidth ['R';'R';'R';'R';'R';'R'] initialPlayerState
    let expectedState = { CurrentDirection = South; CurrentPosition = {X=0; Y=0}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerLM() = 
    let newPlayerState = applyMoves boardWidth ['L';'M'] initialPlayerState
    let expectedState = { CurrentDirection = West; CurrentPosition = {X=0; Y=0}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)

[<Test>]
let MovePlayerLLM() = 
    let newPlayerState = applyMoves boardWidth ['L';'L';'M'] initialPlayerState
    let expectedState = { CurrentDirection = South; CurrentPosition = {X=0; Y=0}}
    Assert.AreEqual(newPlayerState, expectedState, describeStates newPlayerState expectedState)
