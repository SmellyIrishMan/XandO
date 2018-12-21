﻿module Board

type State = Available | PLAYER1 | PLAYER2

let createEmptyBoard =
  Array2D.create<State> 3 3 State.Available

let markPosition (board : State[,]) row col (state : State) =
  Array2D.init<State> 3 3 (fun r c -> if (r, c) = (row, col) then state
                                      else board.[r, c])


let positionIsTaken (board : State[,]) row col =
  let state = board.[row, col]
  match state with
  | State.Available -> false
  | State.PLAYER1 -> true
  | State.PLAYER2 -> true