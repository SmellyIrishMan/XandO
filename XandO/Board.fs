module Board

type State = AVAILABLE | PLAYER1 | PLAYER2
type XandOBoard = State[,]

let createEmptyBoard () =
  Array2D.create<State> 3 3 State.AVAILABLE

let createBoardFromList (list : State[]) =
  let dimension = 3
  Array2D.init<State> dimension dimension (fun row column -> list.[(row*dimension) + column])

let positionIsTaken (board : XandOBoard) row col =
  let state = board.[row, col]
  match state with
  | State.PLAYER1 -> true
  | State.PLAYER2 -> true
  | State.AVAILABLE -> false

//let playerHasWon (board : State[,]) =
  
let lineIsWonByPlayer line (player : State) =
  match player with
  | State.AVAILABLE -> false
  | _ -> List.forall ( fun state -> state = player ) line