module Katas.BoardGame

type Direction = 
    | North
    | East
    | South
    | West

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

type Position = {
    X: int;
    Y: int}

type Player = {
    CurrentPosition: Position;
    CurrentDirection: Direction;}

let currentPlayer = {
    CurrentPosition = { X=0; Y=0 }
    CurrentDirection = North
    }
