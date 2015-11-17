module Katas.BoardGame

type Direction = 
    | North
    | East
    | South
    | West

type Position = {
    X: int;
    Y: int
    }

type PlayerState = {
    CurrentPosition: Position;
    CurrentDirection: Direction
    }

let turnRight d = 
    match d with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft d = 
    match d with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let turnPlayerRight playerState = { playerState with CurrentDirection = turnRight playerState.CurrentDirection }

let turnPlayerLeft playerState = { playerState with CurrentDirection = turnLeft playerState.CurrentDirection }

let returnValidPlayerState boardWidth oldPlayerState newPlayerState = 
    if (newPlayerState.CurrentPosition.Y > boardWidth) ||
        (newPlayerState.CurrentPosition.Y < 0) ||
        (newPlayerState.CurrentPosition.X > boardWidth) ||
        (newPlayerState.CurrentPosition.X < 0) then
        oldPlayerState
    else
        newPlayerState

let movePlayer boardWidth playerState = 
    match playerState.CurrentDirection with
    | North -> 
        let newPlayerState = { playerState with CurrentPosition = { X = playerState.CurrentPosition.X; Y = playerState.CurrentPosition.Y + 1 } }
        returnValidPlayerState boardWidth playerState newPlayerState
    | East -> 
        let newPlayerState = { playerState with CurrentPosition = { X = playerState.CurrentPosition.X + 1; Y = playerState.CurrentPosition.Y } }
        returnValidPlayerState boardWidth playerState newPlayerState
    | South -> 
        let newPlayerState = { playerState with CurrentPosition = { X = playerState.CurrentPosition.X; Y = playerState.CurrentPosition.Y - 1 } }
        returnValidPlayerState boardWidth playerState newPlayerState
    | West -> 
        let newPlayerState = { playerState with CurrentPosition = { X = playerState.CurrentPosition.X - 1; Y = playerState.CurrentPosition.Y } }
        returnValidPlayerState boardWidth playerState newPlayerState

let rec applyMoves boardWidth moves playerState =  
    match moves with
    | [] -> playerState
    | move::nextMoves -> 
        match move with 
        | 'M' -> applyMoves boardWidth nextMoves (movePlayer boardWidth playerState)
        | 'L' -> applyMoves boardWidth nextMoves (turnPlayerLeft playerState)
        | 'R' -> applyMoves boardWidth nextMoves (turnPlayerRight playerState)
        | _   -> applyMoves boardWidth nextMoves playerState 
