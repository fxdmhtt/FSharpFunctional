module Ramda.Tests.LogicTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Ramda

[<Fact>]
let ``test_and`` () =
    ``and`` true true |> should equal true
    ``and`` true false |> should equal false
    ``and`` false true |> should equal false
    ``and`` false false |> should equal false

[<Fact>]
let ``test_complement`` () =
    let isNotNil = complement isNull
    isNull null |> should equal true
    isNotNil null |> should equal false

[<Fact>]
let ``test_cond`` () =
    let fn =
        cond
            [ equals 0, fun _ -> "water freezes at 0°C"
              equals 100, fun _ -> "water boils at 100°C"
              (fun _ -> true), fun temp -> $"nothing special happens at {temp}°C" ]

    fn 0 |> should equal "water freezes at 0°C"
    fn 50 |> should equal "nothing special happens at 50°C"
    fn 100 |> should equal "water boils at 100°C"

[<Fact>]
let ``test_not`` () =
    not true |> should equal false
    not false |> should equal true

[<Fact>]
let ``test_or`` () =
    ``or`` true true |> should equal true
    ``or`` true false |> should equal true
    ``or`` false true |> should equal true
    ``or`` false false |> should equal false

[<Fact>]
let ``test_xor`` () =
    xor true true |> should equal false
    xor true false |> should equal true
    xor false true |> should equal true
    xor false false |> should equal false
