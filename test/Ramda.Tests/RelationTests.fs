module RelationTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Ramda

[<Fact>]
let ``test_clamp`` () =
    clamp 1 10 -5 |> should equal 1
    clamp 1 10 15 |> should equal 10
    clamp 1 10 4 |> should equal 4

[<Fact>]
let ``test_countBy`` () =
    let numbers = [ 1.0; 1.1; 1.2; 2.0; 3.0; 2.2 ]
    countBy Double.Floor numbers |> should equal (Map [ 1.0, 3; 2.0, 2; 3.0, 1 ])

    let letters = [ "a"; "b"; "A"; "a"; "B"; "c" ]
    countBy toLower letters |> should equal (Map [ "a", 3; "b", 2; "c", 1 ])

[<Fact>]
let ``test_difference`` () =
    difference [ 1; 2; 3; 4 ] [ 7; 6; 5; 4; 3 ]
    |> Seq.toList
    |> should equal [ 1; 2 ]

    difference [ 7; 6; 5; 4; 3 ] [ 1; 2; 3; 4 ]
    |> Seq.toList
    |> should equal [ 7; 6; 5 ]

    difference [ Map [ "a", 1 ]; Map["b", 2] ] [ Map [ "a", 1 ]; Map [ "c", 3 ] ]
    |> Seq.toList
    |> should equal [ Map [ "b", 2 ] ]

[<Fact>]
let ``test_differenceWith`` () =
    let cmp (x: Map<string, int>) (y: Map<string, int>) = x.["a"] = y.["a"]
    let l1 = [ Map [ "a", 1 ]; Map [ "a", 2 ]; Map [ "a", 3 ] ]
    let l2 = [ Map [ "a", 3 ]; Map [ "a", 4 ] ]

    differenceWith cmp l1 l2
    |> Seq.toList
    |> should equal [ Map [ "a", 1 ]; Map [ "a", 2 ] ]

[<Fact>]
let ``test_eqBy`` () = eqBy abs 5 -5 |> should equal true

[<Fact>]
let ``test_equals`` () =
    equals 1 1 |> should equal true
    equals [ 1; 2; 3 ] [ 1; 2; 3 ] |> should equal true

    let a = {| v = Map [] |}
    let b = {| v = Map [] |}
    equals a b |> should equal true

[<Fact>]
let ``test_gt`` () =
    gt 2 1 |> should equal true
    gt 2 2 |> should equal false
    gt 2 3 |> should equal false
    gt 'a' 'z' |> should equal false
    gt 'z' 'a' |> should equal true

[<Fact>]
let ``test_gte`` () =
    gte 2 1 |> should equal true
    gte 2 2 |> should equal true
    gte 2 3 |> should equal false
    gte 'a' 'z' |> should equal false
    gte 'z' 'a' |> should equal true

[<Fact>]
let ``test_innerJoin`` () =
    innerJoin
        (fun (record: {| id: int; name: string |}) id -> record.id = id)
        [ {| id = 824; name = "Richie Furay" |}
          {| id = 956; name = "Dewey Martin" |}
          {| id = 313; name = "Bruce Palmer" |}
          {| id = 456; name = "Stephen Stills" |}
          {| id = 177; name = "Neil Young" |} ]
        [ 177; 456; 999 ]
    |> Seq.toList
    |> should equal [ {| id = 456; name = "Stephen Stills" |}; {| id = 177; name = "Neil Young" |} ]

[<Fact>]
let ``test_intersection`` () =
    intersection [ 1; 2; 3; 4 ] [ 7; 6; 5; 4; 3 ] |> should equal [ 4; 3 ]

[<Fact>]
let ``test_lt`` () =
    lt 2 1 |> should equal false
    lt 2 2 |> should equal false
    lt 2 3 |> should equal true
    lt 'a' 'z' |> should equal true
    lt 'z' 'a' |> should equal false

[<Fact>]
let ``test_lte`` () =
    lte 2 1 |> should equal false
    lte 2 2 |> should equal true
    lte 2 3 |> should equal true
    lte 'a' 'z' |> should equal true
    lte 'z' 'a' |> should equal false

[<Fact>]
let ``test_max`` () =
    max 789 123 |> should equal 789
    max 'a' 'b' |> should equal 'b'

[<Fact>]
let ``test_maxBy`` () =
    let square n = n * n
    maxBy square -3 2 |> should equal -3
    reduce (maxBy square) 0 [ 3; -5; 4; 1; -2 ] |> should equal -5
    reduce (maxBy square) 0 [] |> should equal 0

[<Fact>]
let ``test_min`` () =
    min 789 123 |> should equal 123
    min 'a' 'b' |> should equal 'a'

[<Fact>]
let ``test_minBy`` () =
    let square n = n * n
    minBy square -3 2 |> should equal 2
    reduce (minBy square) Int32.MaxValue [ 3; -5; 4; 1; -2 ] |> should equal 1
    reduce (minBy square) Int32.MaxValue [] |> should equal Int32.MaxValue

[<Fact>]
let ``test_sortWith`` () =
    let alice = {| name = "alice"; age = 40 |}
    let bob = {| name = "bob"; age = 30 |}
    let clara = {| name = "clara"; age = 40 |}
    let people = [ alice; bob; clara ]

    let ageNameSort =
        sortWith
            [ descend (fun (x: {| age: int; name: string |}) -> x.age)
              ascend (fun (x: {| age: int; name: string |}) -> x.name) ]

    ageNameSort people |> should equal [ alice; clara; bob ]

[<Fact>]
let ``test_symmetricDifference`` () =
    symmetricDifference [ 1; 2; 3; 4 ] [ 7; 6; 5; 4; 3 ]
    |> should equal [ 1; 2; 7; 6; 5 ]

    symmetricDifference [ 7; 6; 5; 4; 3 ] [ 1; 2; 3; 4 ]
    |> should equal [ 7; 6; 5; 1; 2 ]

[<Fact>]
let ``test_symmetricDifferenceWith`` () =
    let eqA = eqBy (prop "a")
    let l1 = [ Map [ "a", 1 ]; Map [ "a", 2 ]; Map [ "a", 3 ]; Map [ "a", 4 ] ]
    let l2 = [ Map [ "a", 3 ]; Map [ "a", 4 ]; Map [ "a", 5 ]; Map [ "a", 6 ] ]

    symmetricDifferenceWith eqA l1 l2
    |> should equal [ Map [ "a", 1 ]; Map [ "a", 2 ]; Map [ "a", 5 ]; Map [ "a", 6 ] ]

[<Fact>]
let ``test_union`` () =
    union [ 1; 2; 3 ] [ 2; 3; 4 ] |> should equal [ 1; 2; 3; 4 ]

[<Fact>]
let ``test_unionWith`` () =
    let l1 = [ Map [ "a", 1 ]; Map [ "a", 2 ] ]
    let l2 = [ Map [ "a", 1 ]; Map [ "a", 4 ] ]

    unionWith (eqBy (prop "a")) l1 l2
    |> should equal [ Map [ "a", 1 ]; Map [ "a", 2 ]; Map [ "a", 4 ] ]
