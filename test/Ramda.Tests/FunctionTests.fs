module Ramda.Tests.FunctionTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Ramda

[<Fact>]
let ``test_always`` () =
    let t = always "Tee"
    t () |> should equal "Tee"

[<Fact>]
let ``test_ap`` () =
    ap [ multiply 2; add 3 ] [ 1; 2; 3 ] |> should equal [ 2; 4; 6; 4; 5; 6 ]

    ap [ concat "tasty "; toUpper ] [ "pizza"; "salad" ]
    |> should equal [ "tasty pizza"; "tasty salad"; "PIZZA"; "SALAD" ]

[<Fact>]
let ``test_applyTo`` () =
    let t42 = applyTo 42
    t42 id |> should equal 42
    t42 (add 1) |> should equal 43

    applyTo 3 (multiply 2) |> should equal 6
    applyTo "hello" toUpper |> should equal "HELLO"

[<Fact>]
let ``test_ascend`` () =
    let byAge = ascend (fun (x: {| name: string; age: int |}) -> x.age)

    let people =
        [ {| name = "Emma"; age = 70 |}
          {| name = "Peter"; age = 78 |}
          {| name = "Mikhail"; age = 62 |} ]

    let peopleByYoungestFirst = sort byAge people

    peopleByYoungestFirst
    |> should
        equal
        [ {| name = "Mikhail"; age = 62 |}
          {| name = "Emma"; age = 70 |}
          {| name = "Peter"; age = 78 |} ]

[<Fact>]
let ``test_comparator`` () =
    let byAge =
        comparator (fun (a: {| name: string; age: int |}) (b: {| name: string; age: int |}) -> a.age < b.age)

    let people =
        [ {| name = "Emma"; age = 70 |}
          {| name = "Peter"; age = 78 |}
          {| name = "Mikhail"; age = 62 |} ]

    let peopleByIncreasingAge = sort byAge people

    peopleByIncreasingAge
    |> should
        equal
        [ {| name = "Mikhail"; age = 62 |}
          {| name = "Emma"; age = 70 |}
          {| name = "Peter"; age = 78 |} ]

[<Fact>]
let ``test_descend`` () =
    let byAge = descend (fun (x: {| name: string; age: int |}) -> x.age)

    let people =
        [ {| name = "Emma"; age = 70 |}
          {| name = "Peter"; age = 78 |}
          {| name = "Mikhail"; age = 62 |} ]

    let peopleByOldestFirst = sort byAge people

    peopleByOldestFirst
    |> should
        equal
        [ {| name = "Peter"; age = 78 |}
          {| name = "Emma"; age = 70 |}
          {| name = "Mikhail"; age = 62 |} ]

[<Fact>]
let ``test_F`` () = F() |> should equal false

[<Fact>]
let ``test_identity`` () =
    identity 1 |> should equal 1

    let obj = obj ()
    identity obj |> should equal obj

    identity 42 |> should equal 42
    identity "hello" |> should equal "hello"
    identity [ 1; 2; 3 ] |> should equal [ 1; 2; 3 ]

[<Fact>]
let ``test_of`` () =
    ``of`` null |> should equal [ null ]
    ``of`` [ 42 ] |> should equal [ [ 42 ] ]

    ``of`` 42 |> should equal [ 42 ]
    ``of`` "hello" |> should equal [ "hello" ]
    ``of`` [ 1; 2; 3 ] |> should equal [ [ 1; 2; 3 ] ]

[<Fact>]
let ``test_on`` () =
    let eqBy = on (fun a b -> a = b)
    eqBy (prop "a") (Map [ "b", 0; "a", 1 ]) (Map [ "a", 1 ]) |> should equal true

    let add x y = x + y
    let double x = x * 2

    on add double 3 4 |> should equal 14
    on add double 5 6 |> should equal 22

[<Fact>]
let ``test_once`` () =
    let addOneOnce = once (fun x -> x + 1)
    addOneOnce 10 |> should equal 11
    addOneOnce (addOneOnce 50) |> should equal 11

[<Fact>]
let ``test_T`` () = T() |> should equal true
