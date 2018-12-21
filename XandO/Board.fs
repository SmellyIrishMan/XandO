module Board

type State = AVAILABLE | PLAYER1 | PLAYER2

let createEmptyBoard =
  Array2D.create<State> 3 3 State.AVAILABLE

let markPosition (board : State[,]) row col (state : State) =
  Array2D.init<State> 3 3 (fun r c -> if (r, c) = (row, col) then state
                                      else board.[r, c])


let positionIsTaken (board : State[,]) row col =
  let state = board.[row, col]
  match state with
  | State.AVAILABLE -> false
  | State.PLAYER1 -> true
  | State.PLAYER2 -> true

//let playerHasWon (board : State[,]) =
  
let rowIsWonByPlayer row (player : State) =
  match player with
  | State.AVAILABLE -> false
  | _ -> List.forall ( fun state -> state = player ) row