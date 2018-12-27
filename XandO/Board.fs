module Board

type State = AVAILABLE | PLAYER1 | PLAYER2
type XandOBoard = State[,]

let createEmptyBoard =
  Array2D.create<State> 3 3 State.AVAILABLE

let markPosition (board :XandOBoard) row col (state : State) =
  Array2D.init<State> 3 3 (fun r c -> if (r, c) = (row, col) then state
                                      else board.[r, c])

let positionIsTaken (board : XandOBoard) row col =
  let state = board.[row, col]
  match state with
  | State.PLAYER1 -> true
  | State.PLAYER2 -> true
  | State.AVAILABLE -> false

//let playerHasWon (board : State[,]) =
  
let rowIsWonByPlayer row (player : State) =
  match player with
  | State.AVAILABLE -> false
  | _ -> List.forall ( fun state -> state = player ) row