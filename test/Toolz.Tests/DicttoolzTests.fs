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

[<Fact>]
let ``test_dissoc`` () =
    dissoc (Map [ "x", 1; "y", 2 ]) ["y"] |> should equal (Map [ "x", 1 ])
    dissoc (Map [ "x", 1; "y", 2 ]) ["y"; "x"] |> should equal (Map ([]: (string * int) list))
    dissoc (Map [ "x", 1 ]) ["y"] |> should equal (Map [ "x", 1])

    dissoc (Map [ "a", 1 ]) ["a"] |> should equal (Map ([]: (string * int) list))
    dissoc (Map [ "a", 1; "b", 2 ]) ["a"] |> should equal (Map [ "b", 2 ])
    dissoc (Map [ "a", 1; "b", 2 ]) ["b"] |> should equal (Map [ "a", 1 ])
    dissoc (Map [ "a", 1; "b", 2 ]) ["a"; "b"] |> should equal (Map ([]: (string * int) list))
    dissoc (Map [ "a", 1 ]) ["a"] |> should equal (dissoc (dissoc (Map [ "a", 1 ]) ["a"]) ["a"])
    
[<Fact>]
let ``test_itemfilter`` () =
    let isvalid item =
        let k, v = item
        k % 2 = 0 && v < 4

    let d = Map [ 1, 2; 2, 3; 3, 4; 4, 5 ]
    itemfilter isvalid d |> should equal (Map [ 2, 3 ])

    itemfilter (fun item -> iseven(fst item)) (Map [ 1, 2; 2, 3 ]) |> should equal (Map [ 2, 3 ])
    itemfilter (fun item -> iseven(snd item)) (Map [ 1, 2; 2, 3 ]) |> should equal (Map [ 1, 2 ])

[<Fact>]
let ``test_itemmap`` () =
    let accountids = Map [ "Alice", 10; "Bob", 20 ]
    itemmap (fun (a, b) -> (b, a)) accountids |> should equal (Map [ 10, "Alice"; 20, "Bob" ])

    itemmap (fun (a, b) -> (b, a)) (Map [ 1, 2; 2, 4 ]) |> should equal (Map [ 2, 1; 4, 2 ])

[<Fact>]
let ``test_keyfilter`` () =
    let iseven x = x % 2 = 0
    let d = Map [ 1, 2; 2, 3; 3, 4; 4, 5 ]
    keyfilter iseven d |> should equal (Map [ 2, 3; 4, 5 ])

    keyfilter iseven (Map [ 1, 2; 2, 3 ]) |> should equal (Map [ 2, 3 ])

[<Fact>]
let ``test_keymap`` () =
    let bills = Map [ "Alice", [ 20; 15; 30 ]; "Bob", [ 10; 35 ] ]
    keymap (fun (k: string) -> k.ToLower()) bills |> should equal (Map [ "alice", [ 20; 15; 30 ]; "bob", [ 10; 35 ] ])

    keymap inc (Map [ 1, 1; 2, 2 ]) |> should equal (Map [ 2, 1; 3, 2 ])

[<Fact>]
let ``test_merge`` () =
    merge [ Map [ 1, "one" ]; Map [ 2, "two" ] ] |> should equal (Map [ 1, "one"; 2, "two" ])
    merge [ Map [ 1, 2; 3, 4 ]; Map [ 3, 3; 4, 4 ] ] |> should equal (Map [ 1, 2; 3, 3; 4, 4 ])

    merge [ Map [ 1, 1; 2, 2 ]; Map [ 3, 4 ] ] |> should equal (Map [ 1, 1; 2, 2; 3, 4 ])

[<Fact>]
let ``test_merge_with`` () =
    merge_with sum [ Map [ 1, 1; 2, 2 ]; Map [ 1, 10; 2, 20 ] ] |> should equal (Map [ 1, 11; 2, 22 ])
    merge_with head [ Map [ 1, 1; 2, 2 ]; Map [ 2, 20; 3, 30 ] ] |> should equal (Map [ 1, 1; 2, 2; 3, 30 ])

    let dicts = [ Map [ 1, 1; 2, 2 ]; Map [ 1, 10; 2, 20 ] ]
    merge_with sum dicts |> should equal (Map [ 1, 11; 2, 22 ])
    merge_with toTuple dicts |> should equal (Map [ 1, (1, 10); 2, (2, 20) ])

    let dicts = [ Map [ 1, 1; 2, 2; 3, 3 ]; Map [ 1, 10; 2, 20 ] ]
    merge_with sum dicts |> should equal (Map [ 1, 11; 2, 22; 3, 3 ])
    merge_with toTuple dicts |> should equal (Map [ 1, (1, 10); 2, (2, 20); 3, (3,) ])

    merge_with sum [] |> length |> should equal 0

[<Fact>]
let ``test_valfilter`` () =
    let iseven x = x % 2 = 0
    let d = Map [ 1, 2; 2, 3; 3, 4; 4, 5 ]
    valfilter iseven d |> should equal (Map [ 1, 2; 3, 4 ])

    valfilter iseven (Map [ 1, 2; 2, 3 ]) |> should equal (Map [ 1, 2 ])

[<Fact>]
let ``test_valmap`` () =
    let bills = Map [ "Alice", [ 20; 15; 30 ]; "Bob", [ 10; 35 ] ]
    valmap sum bills |> should equal (Map [ "Alice", 65; "Bob", 45 ])

    valmap inc (Map [ 1, 1; 2, 2 ]) |> should equal (Map [ 1, 2; 2, 3 ])
