module Board

type State = AVAILABLE | PLAYER1 | PLAYER2
type Position = int*int //Row, Col
type Direction = int*int
type XandOBoard = State[,]

let createEmptyBoard () =
  Array2D.create<State> 3 3 State.AVAILABLE

let createBoardFromList (list : State[]) =
  let dimension = 3
  Array2D.init<State> dimension dimension (fun row column -> list.[(row*dimension) + column])

let positionIsTaken (board:XandOBoard) (position:Position) =
  let state = board.[fst(position), snd(position)]
  match state with
  | State.PLAYER1 -> true
  | State.PLAYER2 -> true
  | State.AVAILABLE -> false

//let playerHasWon (board : State[,]) =
  
let lineIsWonByPlayer line (player:State) =
  match player with
  | State.AVAILABLE -> false
  | _ -> List.forall ( fun state -> state = player ) line

let positionIsOnBoard (position:Position) rows columns =
  fst(position) >= 0 && fst(position) < rows && snd(position) >= 0 && snd(position) < columns

let getBoardPositionForIteration startPosition direction iteration =
  (fst(startPosition) + (fst(direction) * iteration), snd(startPosition) + (snd(direction) * iteration))

let getLine (board:XandOBoard) (startPosition:Position) (direction:Direction) =
  let rows = Array2D.length1 board
  let columns = Array2D.length2 board

  let aSqr = pown rows rows
  let bSqr = pown columns columns
  let abSqr = aSqr + bSqr
  let maxDiagonal = abSqr |> float |> sqrt |> ceil |> int

  let mutable iterations = 0
  for i = 0 to maxDiagonal do
    let position = getBoardPositionForIteration startPosition direction i
    if positionIsOnBoard position rows columns then
      iterations <- iterations + 1

  [for i in 0 .. (iterations - 1) -> 
    let position = getBoardPositionForIteration startPosition direction i
    board.[fst(position), snd(position)]]

let rec leadingStateCount line checkState =
  match line with
  | state :: restOfStates ->
    if state = checkState then
      (checkState, 1 + snd(leadingStateCount restOfStates checkState))
    else
      (checkState, 0)
  | [] -> (checkState, 0)

let rec getLineStateCounts line =
  if List.isEmpty line then
    []
  else
    let stateCount = leadingStateCount line line.Head
    let remainderOfList = snd(List.splitAt (snd(stateCount)) line)
    List.append [stateCount] (getLineStateCounts remainderOfList)