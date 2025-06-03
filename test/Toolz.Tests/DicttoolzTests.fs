module DicttoolzTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Toolz.Dicttoolz

let identity x = x
let inc x = x + 1
let iseven x = x % 2 = 0

[<Fact>]
let ``test_assoc`` () =
    assoc (Map [ "x", 1 ]) "x" 2 |> should equal (Map [ "x", 2 ])
    assoc (Map [ "x", 1 ]) "y" 3 |> should equal (Map [ "x", 1; "y", 3 ])

    assoc (Map []) "a" 1 |> should equal (Map [ "a", 1 ])
    assoc (Map [ "a", 1 ]) "a" 3 |> should equal (Map [ "a", 3 ])
    assoc (Map [ "a", 1 ]) "b" 3 |> should equal (Map [ "a", 1; "b", 3 ])

    let d = Map [ "x", 1 ]
    let oldd = d
    assoc d "x" 2 |> should not' (equal oldd)