module BoardTests

#if INTERACTIVE
#r @"C:/Program Files (x86)/NUnit.org/framework/3.0.5813.39031/net-4.5/nunit.framework.dll"
#endif

open NUnit.Framework

[<Test>]
let ``Create an empty board``() =
  let board = Board.createEmptyBoard
  let maxRow = (Array2D.length1 board) - 1
  let maxCol = (Array2D.length2 board) - 1

  printfn "%A" board
  for row in 0 .. maxRow do
      for col in 0 .. maxCol do
            Assert.AreEqual(board.[row, col], Board.State.AVAILABLE)

[<Test>]
let ``Get Row and Col of board``() =
  let board = Board.createEmptyBoard
  let row0 = board.[0, *]
  let col2 = board.[*, 2]

  printfn "%A" row0
  printfn "%A" col2
  Assert.AreEqual(row0.Length, 3)
  Assert.AreEqual(col2.Length, 3)

[<Test>]
let ``Mark positions on board``() =
  let board = Board.createEmptyBoard
  let board = Board.markPosition board 0 0 Board.PLAYER1
  let board = Board.markPosition board 1 0 Board.PLAYER2
  let board = Board.markPosition board 2 0 Board.AVAILABLE
  let board = Board.markPosition board 1 1 Board.PLAYER1
  let board = Board.markPosition board 2 2 Board.PLAYER1
  
  printfn "%A" board
  Assert.IsTrue(board.[0, 0] = Board.PLAYER1)
  Assert.IsTrue(board.[2, 0] = Board.AVAILABLE)
  Assert.IsTrue(board.[2, 2] = Board.PLAYER1)
  Assert.IsTrue(board.[1, 0] = Board.PLAYER2)

  Assert.IsFalse(board.[0, 0] = Board.AVAILABLE)
  Assert.IsFalse(board.[1, 0] = Board.PLAYER1)

[<Test>]
let ``A position is taken``() =
  let board = Board.createEmptyBoard
  let board = Board.markPosition board 0 0 Board.PLAYER1
  Assert.IsTrue(Board.positionIsTaken board 0 0)