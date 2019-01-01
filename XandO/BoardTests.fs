module BoardTests

#if INTERACTIVE
#r @"C:/Program Files (x86)/NUnit.org/framework/3.0.5813.39031/net-4.5/nunit.framework.dll"
#endif

open NUnit.Framework

[<Test>]
let ``Create an empty board``() =
  let board = Board.createEmptyBoard()
  let maxRow = (Array2D.length1 board) - 1
  let maxCol = (Array2D.length2 board) - 1

  printfn "%A" board
  for row in 0 .. maxRow do
      for col in 0 .. maxCol do
            Assert.AreEqual(board.[row, col], Board.State.AVAILABLE)

[<Test>]
let ``Define a board``() =
  let values = [|Board.PLAYER1; Board.PLAYER2; Board.AVAILABLE;
                Board.PLAYER2; Board.AVAILABLE; Board.PLAYER2;
                Board.AVAILABLE; Board.PLAYER2; Board.PLAYER1|]
  let board = Board.createBoardFromList values

  printfn "%A" board

  Assert.AreEqual(board.[0, 0], Board.PLAYER1)
  Assert.AreEqual(board.[1, 2], Board.PLAYER2)
  Assert.AreEqual(board.[2, 0], Board.AVAILABLE)

[<Test>]
let ``Get Row and Col of board``() =
  let board = Board.createEmptyBoard()
  let row0 = board.[0, *]
  let col2 = board.[*, 2]

  printfn "%A" row0
  printfn "%A" col2
  Assert.AreEqual(row0.Length, 3)
  Assert.AreEqual(col2.Length, 3)

[<Test>]
let ``Mark positions on board``() =
  let values = [|Board.PLAYER1; Board.AVAILABLE; Board.AVAILABLE;
                Board.PLAYER2; Board.PLAYER1; Board.AVAILABLE;
                Board.AVAILABLE; Board.AVAILABLE; Board.PLAYER1|]

  let board = Board.createBoardFromList values
  
  printfn "%A" board
  Assert.IsTrue(board.[0, 0] = Board.PLAYER1)
  Assert.IsTrue(board.[2, 0] = Board.AVAILABLE)
  Assert.IsTrue(board.[2, 2] = Board.PLAYER1)
  Assert.IsTrue(board.[1, 0] = Board.PLAYER2)

  Assert.IsFalse(board.[0, 0] = Board.AVAILABLE)
  Assert.IsFalse(board.[1, 0] = Board.PLAYER1)

[<Test>]
let ``A position is taken``() =
  let board = Board.createEmptyBoard()
  board.[0, 0] <- Board.PLAYER1
  Assert.IsTrue(Board.positionIsTaken board (0, 0))

[<Test>]
let ``A line is won``() =
  let values = [|Board.AVAILABLE; Board.PLAYER2; Board.AVAILABLE;
                Board.PLAYER1; Board.PLAYER2; Board.PLAYER1;
                Board.PLAYER2; Board.PLAYER2; Board.PLAYER2|]

  let board = Board.createBoardFromList values

  printfn "%A" board
  
  let row0List = board.[0, *] |> Array.toList
  Assert.IsFalse(Board.lineIsWonByPlayer row0List Board.AVAILABLE)

  let row1List = board.[1, *] |> Array.toList
  Assert.IsFalse(Board.lineIsWonByPlayer row1List Board.PLAYER1)

  let row2List = board.[2, *] |> Array.toList
  Assert.IsTrue(Board.lineIsWonByPlayer row2List Board.PLAYER2)

  let col0List = board.[*, 0] |> Array.toList
  Assert.IsFalse(Board.lineIsWonByPlayer col0List Board.PLAYER1)
  Assert.IsFalse(Board.lineIsWonByPlayer col0List Board.PLAYER2)

  let col1List = board.[*, 1] |> Array.toList
  Assert.IsFalse(Board.lineIsWonByPlayer col1List Board.PLAYER1)
  Assert.IsTrue(Board.lineIsWonByPlayer col1List Board.PLAYER2)

[<Test>]
let ``Get a line``() =
  let values = [|Board.AVAILABLE; Board.PLAYER2; Board.PLAYER1;
                Board.PLAYER1; Board.PLAYER2; Board.PLAYER1;
                Board.PLAYER2; Board.PLAYER2; Board.PLAYER2|]

  let board = Board.createBoardFromList values
  printfn "%A" board
  
  let line = Board.getLine board (0,0) (1,0)
  Assert.AreEqual(line, board.[*, 0])

  let line = Board.getLine board (2,0) (0,1)
  Assert.AreEqual(line, board.[2, *])

  let line = Board.getLine board (2,0) (1,1)
  Assert.AreEqual(line, [Board.PLAYER2])

  let line = Board.getLine board (0,0) (1,1)
  Assert.AreEqual(line, [Board.AVAILABLE; Board.PLAYER2; Board.PLAYER2; ])

  let line = Board.getLine board (0,0) (-1,-1)
  Assert.AreEqual(line, [Board.AVAILABLE ])

  let line = Board.getLine board (2,2) (-1,-1)
  Assert.AreEqual(line, [Board.PLAYER2; Board.PLAYER2; Board.AVAILABLE; ])

  let line = Board.getLine board (0,2) (1,-1)
  Assert.AreEqual(line, [Board.PLAYER1; Board.PLAYER2; Board.PLAYER2; ])

[<Test>]
let ``LeadingStateCount``() =
  let values = [Board.AVAILABLE; Board.AVAILABLE; Board.AVAILABLE; Board.PLAYER2; Board.PLAYER1;]
  let stateCount = Board.leadingStateCount values Board.AVAILABLE
  printfn "%A" stateCount
  Assert.AreEqual((stateCount), (Board.AVAILABLE, 3))

  let values = [Board.PLAYER2; Board.PLAYER1;]
  let stateCount = Board.leadingStateCount values Board.AVAILABLE
  printfn "%A" stateCount
  Assert.AreEqual((stateCount), (Board.AVAILABLE, 0))

  let values = [Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1;]
  let stateCount = Board.leadingStateCount values Board.PLAYER1
  printfn "%A" stateCount
  Assert.AreEqual((stateCount), (Board.PLAYER1, 1))

  let values = [Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1;]
  let stateCount = Board.leadingStateCount values Board.PLAYER1
  printfn "%A" stateCount
  Assert.AreEqual((stateCount), (Board.PLAYER1, 6))

[<Test>]
let ``GetLineStateCounts``() =
  let values = [Board.AVAILABLE; Board.AVAILABLE; Board.AVAILABLE; Board.PLAYER2; Board.PLAYER1;]
  let stateCounts = Board.getLineStateCounts values
  printfn "%A" stateCounts
  Assert.AreEqual(stateCounts, [(Board.AVAILABLE, 3); (Board.PLAYER2, 1); (Board.PLAYER1, 1)])

  let values = [Board.PLAYER2; Board.PLAYER1;]
  let stateCounts = Board.getLineStateCounts values
  printfn "%A" stateCounts
  Assert.AreEqual(stateCounts, [(Board.PLAYER2, 1); (Board.PLAYER1, 1)])

  let values = [Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1;]
  let stateCounts = Board.getLineStateCounts values
  printfn "%A" stateCounts
  Assert.AreEqual(stateCounts, [(Board.PLAYER1, 1); (Board.PLAYER2, 1); (Board.PLAYER1, 5)])

  let values = [Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER2; Board.PLAYER1;]
  let stateCounts = Board.getLineStateCounts values
  printfn "%A" stateCounts
  Assert.AreEqual(stateCounts, [(Board.PLAYER1, 1); (Board.PLAYER2, 1); (Board.PLAYER1, 1); (Board.PLAYER2, 1); (Board.PLAYER1, 1); (Board.PLAYER2, 1); (Board.PLAYER1, 1)])

  let values = [Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1;]
  let stateCounts = Board.getLineStateCounts values
  printfn "%A" stateCounts
  Assert.AreEqual(stateCounts, [(Board.PLAYER1, 6)])

[<Test>]
let ``Check if a player won a line``() =
  let values = [Board.AVAILABLE; Board.AVAILABLE; Board.AVAILABLE; Board.PLAYER2; Board.PLAYER1;]
  let winner = Board.checkIfPlayerWonLine values 3
  Assert.IsFalse(fst(winner))

  let values = [Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1;]
  let winner = Board.checkIfPlayerWonLine values 3
  Assert.IsTrue(fst(winner))
  Assert.AreEqual(snd(winner), (Board.PLAYER1, 5))

  let values = [Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER2; Board.PLAYER1; Board.PLAYER2; Board.PLAYER1;]
  let winner = Board.checkIfPlayerWonLine values 3
  Assert.IsFalse(fst(winner))

  let values = [Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1; Board.PLAYER1;]
  let winner = Board.checkIfPlayerWonLine values 3
  Assert.IsTrue(fst(winner))
  Assert.AreEqual(snd(winner), (Board.PLAYER1, 6))


[<Test>]
let ``Check if a player won a board``() =
  let values = [|Board.PLAYER2; Board.PLAYER2; Board.PLAYER1;
                Board.PLAYER2; Board.PLAYER1; Board.AVAILABLE;
                Board.PLAYER1; Board.PLAYER2; Board.PLAYER2|]

  let board = Board.createBoardFromList values
  printfn "Board"
  printfn "%A\n" board
  let winner = Board.IsGameOverAndWhoWon board

  Assert.IsTrue(fst(winner))
  Assert.AreEqual(snd(winner), (Board.PLAYER1, 3))

  let values = [|Board.AVAILABLE; Board.AVAILABLE; Board.AVAILABLE;
                Board.AVAILABLE; Board.AVAILABLE; Board.AVAILABLE;
                Board.AVAILABLE; Board.AVAILABLE; Board.AVAILABLE|]

  let board = Board.createBoardFromList values
  printfn "Board"
  printfn "%A\n" board
  let winner = Board.IsGameOverAndWhoWon board

  Assert.IsFalse(fst(winner))
  Assert.AreEqual(snd(winner), (Board.AVAILABLE, 0))

  let values = [|Board.PLAYER2; Board.PLAYER1; Board.PLAYER1;
                Board.PLAYER1; Board.PLAYER2; Board.PLAYER1;
                Board.PLAYER1; Board.PLAYER1; Board.PLAYER2|]

  let board = Board.createBoardFromList values
  printfn "Board"
  printfn "%A\n" board
  let winner = Board.IsGameOverAndWhoWon board

  Assert.IsTrue(fst(winner))
  Assert.AreEqual(snd(winner), (Board.PLAYER2, 3))