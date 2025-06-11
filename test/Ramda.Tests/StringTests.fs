module StringTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Ramda

[<Fact>]
let ``test_match`` () =
    ``match`` "([a-z]a)" "bananas" |> should equal [ "ba"; "na"; "na" ]
    ``match`` "a" "b" |> should equal ([]: string list)
    (fun () -> ``match`` "a" null |> ignore) |> should throw typeof<ArgumentNullException>

[<Fact>]
let ``test_replace`` () =
    replace "foo" "bar" "foo foo foo" |> should equal "bar bar bar"

[<Fact>]
let ``test_split`` () =
    let pathComponents = split "/"

    tail (pathComponents "/usr/local/bin/node")
    |> toList
    |> should equal [ "usr"; "local"; "bin"; "node" ]

    split "." "a.b.c.xyz.d" |> toList |> should equal [ "a"; "b"; "c"; "xyz"; "d" ]

[<Fact>]
let ``test_test`` () =
    test "^x" "xyz" |> should equal true
    test "^y" "xyz" |> should equal false

[<Fact>]
let ``test_toLower`` () = toLower "XYZ" |> should equal "xyz"

[<Fact>]
let ``test_toString`` () =
    toString 42 |> should equal "42"
    toString "\"abc\"" |> should equal "\"abc\""
    toString [ 1; 2; 3 ] |> should equal "[1; 2; 3]"

    toString {| foo = 1; bar = 2; baz = 3 |}
    |> should equal "{ bar = 2\n  baz = 3\n  foo = 1 }"

    toString (new DateTime(2001, 2, 3, 4, 5, 6, 0))
    |> should equal "2001/2/3 4:05:06"

[<Fact>]
let ``test_toUpper`` () = toUpper "abc" |> should equal "ABC"

[<Fact>]
let ``test_trim`` () =
    trim "   xyz  " |> should equal "xyz"
    map trim (split "," "x, y, z") |> toList |> should equal [ "x"; "y"; "z" ]
