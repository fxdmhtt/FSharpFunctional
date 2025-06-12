module Ramda.Tests.ObjectTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Ramda

[<Fact>]
let ``test_assoc`` () =
    assoc "c" 3 (Map [ "a", 1; "b", 2 ])
    |> should equal (Map [ "a", 1; "b", 2; "c", 3 ])

    assoc "a" 3 (Map [ "a", 1; "b", 2 ]) |> should equal (Map [ "a", 3; "b", 2 ])
    assoc "a" 3 (Map []) |> should equal (Map [ "a", 3 ])

[<Fact>]
let ``test_dissoc`` () =
    dissoc "b" (Map [ "a", 1; "b", 2; "c", 3 ])
    |> should equal (Map [ "a", 1; "c", 3 ])

    dissoc "a" (Map [ "a", 1; "b", 2 ]) |> should equal (Map [ "b", 2 ])

    dissoc "a" (Map([]: list<string * int>))
    |> should equal (Map([]: list<string * int>))

[<Fact>]
let ``test_eqProps`` () =
    let o1 = Map [ "a", 1; "b", 2; "c", 3; "d", 4 ]
    let o2 = Map [ "a", 10; "b", 20; "c", 3; "d", 40 ]
    eqProps "a" o1 o2 |> should equal false
    eqProps "c" o1 o2 |> should equal true

[<Fact>]
let ``test_forEachObjIndexed`` () =
    let originalOut = Console.Out
    use sw = new IO.StringWriter()

    let printKeyConcatValue = fun value key _ -> printfn $"{key}:{value}"

    Console.SetOut(sw)

    forEachObjIndexed printKeyConcatValue (Map [ "x", 1; "y", 2 ])
    |> should equal (Map [ "x", 1; "y", 2 ])

    Console.SetOut(originalOut)

    sw.ToString() |> should equal "x:1\r\ny:2\r\n"

[<Fact>]
let ``test_has`` () =
    let hasName = has "name"
    hasName (Map [ "name", "alice" ]) |> should equal true
    hasName (Map [ "name", "bob" ]) |> should equal true
    hasName (Map []) |> should equal false
    let point = Map [ "x", 0; "y", 0 ]
    let pointHas = flip has point
    pointHas "x" |> should equal true
    pointHas "y" |> should equal true
    pointHas "z" |> should equal false

[<Fact>]
let ``test_invert`` () =
    let raceResultsByFirstName =
        Map [ "first", "alice"; "second", "jake"; "third", "alice" ]

    invert raceResultsByFirstName
    |> should equal (Map [ "alice", [ "first"; "third" ]; "jake", [ "second" ] ])

[<Fact>]
let ``test_invertObj`` () =
    let raceResults = Map [ "first", "alice"; "second", "jake" ]

    invertObj raceResults
    |> should equal (Map [ "alice", "first"; "jake", "second" ])

    let raceResultsByFirstName =
        Map [ "first", "alice"; "second", "jake"; "third", "alice" ]

    invertObj raceResultsByFirstName
    |> should equal (Map [ "alice", "third"; "jake", "second" ])

[<Fact>]
let ``test_keys`` () =
    keys (Map [ "a", 1; "b", 2; "c", 3 ])
    |> toList
    |> should equal [ "a"; "b"; "c" ]

[<Fact>]
let ``test_mapObjIndexed`` () =
    let xyz = Map [ "x", 1; "y", 2; "z", 3 ]
    let prependKeyAndDouble = fun num key obj -> $"{key}{num * 2}"

    mapObjIndexed prependKeyAndDouble xyz
    |> should equal (Map [ "x", "x2"; "y", "y4"; "z", "z6" ])

[<Fact>]
let ``test_mergeLeft`` () =
    mergeLeft (Map [ "age", 40 ]) (Map [ "name", 1234; "age", 10 ])
    |> should equal (Map [ "name", 1234; "age", 40 ])

    let resetToDefault = mergeLeft (Map [ "x", 0 ])
    resetToDefault (Map [ "x", 5; "y", 2 ]) |> should equal (Map [ "x", 0; "y", 2 ])

[<Fact>]
let ``test_mergeRight`` () =
    mergeRight (Map [ "name", 1234; "age", 10 ]) (Map [ "age", 40 ])
    |> should equal (Map [ "name", 1234; "age", 40 ])

    let withDefaults = mergeRight (Map [ "x", 0; "y", 0 ])
    withDefaults (Map [ "y", 2 ]) |> should equal (Map [ "x", 0; "y", 2 ])

[<Fact>]
let ``test_mergeWith`` () =
    mergeWith (@) (Map [ "a", [ 1 ]; "values", [ 10; 20 ] ]) (Map [ "b", [ 1 ]; "values", [ 15; 35 ] ])
    |> should equal (Map [ "a", [ 1 ]; "b", [ 1 ]; "values", [ 10; 20; 15; 35 ] ])

// [<Fact>]
// let ``test_mergeWithKey`` () =
//     let concatValues =
//         fun (k, l, r) ->
//             if k = "values" then
//                 (l :?> int list) @ (r :?> int list)
//             else
//                 r

//     mergeWithKey
//         concatValues
//         (Map [ "a", true; "thing", "foo"; "values", [ 10; 20 ] ])
//         (Map [ "b", true; "thing", "bar"; "values", [ 15; 35 ] ])
//     |> should equal (Map [ "a", true; "b", true; "thing", "bar"; "values", [ 10; 20; 15; 35 ] ])

// [<Fact>]
// let ``test_modify`` () =
//     let person = Map [ "name", "James"; "age", 20; "pets", [ "dog"; "cat" ] ]

//     modify "age" (add 1) person
//     |> should equal (Map [ "name", "James"; "age", 21; "pets", [ "dog"; "cat" ] ])

//     modify "pets" (append "turtle") person
//     |> should equal (Map [ "name", "James"; "age", 20; "pets", [ "dog"; "cat"; "turtle" ] ])

[<Fact>]
let ``test_objOf`` () =
    let matchPhrases = (objOf "must") << (map (objOf "match_phrase"))

    matchPhrases [ "foo"; "bar"; "baz" ]
    |> should
        equal
        (Map
            [ "must",
              [ Map [ "match_phrase", "foo" ]
                Map [ "match_phrase", "bar" ]
                Map [ "match_phrase", "baz" ] ] ])

[<Fact>]
let ``test_omit`` () =
    omit [ "a"; "d" ] (Map [ "a", 1; "b", 2; "c", 3; "d", 4 ])
    |> should equal (Map [ "b", 2; "c", 3 ])

[<Fact>]
let ``test_pick`` () =
    pick [ "a"; "d" ] (Map [ "a", 1; "b", 2; "c", 3; "d", 4 ])
    |> should equal (Map [ "a", 1; "d", 4 ])

    pick [ "a"; "e"; "f" ] (Map [ "a", 1; "b", 2; "c", 3; "d", 4 ])
    |> should equal (Map [ "a", 1 ])

[<Fact>]
let ``test_pickAll`` () =
    pickAll [ "a"; "d" ] (Map [ "a", 1; "b", 2; "c", 3; "d", 4 ])
    |> should equal (Map [ "a", Some 1; "d", Some 4 ])

    pickAll [ "a"; "e"; "f" ] (Map [ "a", 1; "b", 2; "c", 3; "d", 4 ])
    |> should equal (Map [ "a", Some 1; "e", None; "f", None ])

[<Fact>]
let ``test_pickBy`` () =
    let isUpperCase = fun ``val`` (key: string) -> key.ToUpperInvariant() = key

    pickBy isUpperCase (Map [ "a", 1; "b", 2; "A", 3; "B", 4 ])
    |> should equal (Map [ "A", 3; "B", 4 ])

// [<Fact>]
// let ``test_project`` =
//     let abby = Map [ "name", "Abby"; "age", 7; "hair", "blond"; "grade", 2 ]
//     let fred = Map [ "name", "Fred"; "age", 12; "hair", "brown"; "grade", 7 ]
//     let kids = [ abby; fred ]

//     project [ "name"; "grade" ] kids
//     |> should equal [ Map [ "name", "Abby"; "grade", 2 ]; Map [ "name", "Fred"; "grade", 7 ] ]

[<Fact>]
let ``test_prop`` () =
    prop "x" (Map [ "x", 100 ]) |> should equal (Some 100)
    prop "x" (Map []) |> should equal None
    prop 0 [ 100 ] |> should equal (Some 100)
    (Option.map inc << prop "x") (Map [ "x", 3 ]) |> should equal (Some 4)

// [<Fact>]
// let ``test_propOr`` () =
//     let alice = Map [ "name", "ALICE"; "age", 101 ]
//     let favorite = prop "favoriteLibrary"
//     let favoriteWithDefault = propOr "Ramda" "favoriteLibrary"
//     favorite alice |> should equal None
//     favoriteWithDefault alice |> should equal "Ramda"

// [<Fact>]
// let ``test_props`` () =
//     props [ "x"; "y" ] (Map [ "x", 1; "y", 2 ]) |> should equal [ Some 1; Some 2 ]

//     props [ "c"; "a"; "b" ] (Map [ "b", 2; "a", 1 ])
//     |> should equal [ None; Some 1; Some 2 ]

//     let fullName = (join " ") << (props [ "first"; "last" ])

//     fullName (Map [ "last", "Bullet-Tooth"; "age", 33; "first", "Tony" ])
//     |> should equal "Tony Bullet-Tooth"

// [<Fact>]
// let ``test_toPairs`` () =
//     toPairs (Map [ "a", 1; "b", 2; "c", 3 ])
//     |> should equal [ "a", 1; "b", 2; "c", 3 ]

// [<Fact>]
// let ``test_unwind`` () =
//     unwind
//         "hobbies"
//         (Map
//             [ "name", "alice"
//               "hobbies", [ "Golf"; "Hacking" ]
//               "colors", [ "red"; "green" ] ])
//     |> should
//         equal
//         [ Map [ "name", "alice"; "hobbies", "Golf"; "colors", [ "red"; "green" ] ]
//           Map [ "name", "alice"; "hobbies", "Hacking"; "colors", [ "red"; "green" ] ] ]

// [<Fact>]
// let ``test_where`` () =
//     let pred = where
//         (Map [ "a", equals "foo"
//                "b", complement (equals "bar")
//                "x", flip gt 10
//                "y", flip lt 20 ])
//     pred (Map [ "a", "foo"; "b", "xxx"; "x", 11; "y", 19 ]) |> should equal true
//     pred (Map [ "a", "xxx"; "b", "xxx"; "x", 11; "y", 19 ]) |> should equal false
//     pred (Map [ "a", "foo"; "b", "bar"; "x", 11; "y", 19 ]) |> should equal false
//     pred (Map [ "a", "foo"; "b", "xxx"; "x", 10; "y", 19 ]) |> should equal false
//     pred (Map [ "a", "foo"; "b", "xxx"; "x", 11; "y", 20 ]) |> should equal false

// [<Fact>]
// let ``test_whereAny`` () =
//     let pred = whereAny
//         (Map [ "a", equals "foo"
//                "b", complement (equals "xxx")
//                "x", flip gt 10
//                "y", flip lt 20 ])

//     pred (Map [ "a", "foo"; "b", "xxx"; "x", 8; "y", 34 ]) |> should equal true
//     pred (Map [ "a", "xxx"; "b", "xxx"; "x", 9; "y", 21 ]) |> should equal false
//     pred (Map [ "a", "bar"; "b", "xxx"; "x", 10; "y", 20 ]) |> should equal false
//     pred (Map [ "a", "foo"; "b", "bar"; "x", 10; "y", 20 ]) |> should equal true
//     pred (Map [ "a", "foo"; "b", "xxx"; "x", 11; "y", 20 ]) |> should equal true

[<Fact>]
let ``test_whereEq`` () =
    let pred = whereEq (Map [ "a", 1; "b", 2 ])

    pred (Map [ "a", 1 ]) |> should equal false
    pred (Map [ "a", 1; "b", 2 ]) |> should equal true
    pred (Map [ "a", 1; "b", 2; "c", 3 ]) |> should equal true
    pred (Map [ "a", 1; "b", 1 ]) |> should equal false
