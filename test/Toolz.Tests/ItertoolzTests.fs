module ItertoolzTests

open System
open Xunit
open FsUnit.Xunit
open FsCheck.Xunit
open operator
open Toolz.Itertoolz

let identity x = x
let iseven x = x % 2 = 0
let isodd x = x % 2 = 1
let inc x = x + 1
let double x = 2 * x

[<Fact>]
let ``test_accumulate`` () =
    accumulate add [1; 2; 3; 4; 5] None |> should equal [1; 3; 6; 10; 15]
    accumulate mul [1; 2; 3; 4; 5] None |> should equal [1; 2; 6; 24; 120]
    accumulate add [1; 2; 3; 4; 5] (Some -1) |> should equal [-1; 0; 2; 5; 9; 14]

    let binop a b =
        System.Diagnostics.Debug.Assert(false, "binop should not be called")
        0

    // start = object()
    // assert list(accumulate(binop, [], start)) == [start]
    accumulate binop [] None |> should equal ([] |> List.map ignore)
    // assert list(accumulate(add, [1, 2, 3], no_default2)) == [1, 3, 6]
