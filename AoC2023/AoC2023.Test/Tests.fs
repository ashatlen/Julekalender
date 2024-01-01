module AoC2023.Test

open System
open NUnit.Framework
open AoC2023.Luke

[<TestFixture>]
type Tests () = 

    [<SetUp>]
    member public x.setup() =
        ()

    [<Test>]
    member public x.``December 10 1 test`` () =
        let expected = 8
        let actual = Luke10._1 "../../../../Inputs/dec10t.txt"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public x.``December 10 1`` () =
        let expected = 6968
        let actual = Luke10._1 "../../../../Inputs/dec10.txt"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public x.``December 10 2 test`` () =

        let expected = -1
        let actual = Luke10._2 "../../../../Inputs/dec10t.txt"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public x.``December 10 2`` () =

        let expected = -1
        let actual = Luke10._2 "../../../../Inputs/dec10.txt"
        Assert.AreEqual(expected, actual)
