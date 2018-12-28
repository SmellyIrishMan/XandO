module SimpleTests

#if INTERACTIVE
#r @"C:/Program Files (x86)/NUnit.org/framework/3.0.5813.39031/net-4.5/nunit.framework.dll"
#endif

open NUnit.Framework

[<Test>]
let ``1 = 1``() =
  Assert.AreEqual(1, 1)

[<Test>]
let ``Array from 1 to 10``() =
  let size = 10
  let array = [| for i in 1 .. size -> i |]
  printfn "%A" array
  Assert.AreEqual(array.Length, size)

[<Test>]
let ``Array of Arrays``() =
  let arrayOfArrays = [| [| 1; 2; 3 |]; [| 13; 14; 15; 16; 17 |]; [| 101; 102; 103; 104; 105; 666; 666; 666; 666 |] |]
  printfn "%A" arrayOfArrays
  Assert.AreEqual(Array.length arrayOfArrays.[0], 3)
  Assert.AreEqual(Array.length arrayOfArrays.[1], 5)
  Assert.AreEqual(Array.length arrayOfArrays.[2], 9)
  Assert.AreEqual(arrayOfArrays.[2].[5], 666)
  Assert.AreEqual(arrayOfArrays.[2].[6], 666)
  Assert.AreEqual(arrayOfArrays.[2].[7], 666)

[<Test>]
let ``2D Array``() =
  let twoDArray = Array2D.create<int> 3 3 1

  printfn "%A" twoDArray
  Assert.AreEqual(Array2D.length1 twoDArray, 3)
  Assert.AreEqual(Array2D.length2 twoDArray, 3)

[<Test>]
let ``2D Array Out Of Bounds``() =
  let twoDArray = Array2D.create<int> 2 2 1

  Assert.Throws<System.IndexOutOfRangeException>(fun() -> twoDArray.[1000, 1000] |> ignore)
  |> ignore