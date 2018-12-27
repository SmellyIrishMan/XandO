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
  Assert.IsTrue(Board.positionIsTaken board 0 0)

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
let ``Define a board``() =
  let values = [|Board.PLAYER1; Board.PLAYER2; Board.AVAILABLE;
                Board.PLAYER2; Board.AVAILABLE; Board.PLAYER2;
                Board.AVAILABLE; Board.PLAYER2; Board.PLAYER1|]

  let board = Board.createBoardFromList values

  printfn "%A" board

  Assert.AreEqual(board.[0, 0], Board.PLAYER1)
  Assert.AreEqual(board.[1, 2], Board.PLAYER2)
  Assert.AreEqual(board.[2, 0], Board.AVAILABLE)