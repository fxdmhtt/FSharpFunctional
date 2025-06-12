module Toolz.Tests.RecipesTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Toolz

let iseven x = x % 2 = 0

[<Fact>]
let ``test_countby`` () =
    countby iseven [ 1; 2; 3 ] |> should equal (Map [ (true, 1); (false, 2) ])

    countby length [ "cat"; "dog"; "mouse" ]
    |> should equal (Map [ (3, 2); (5, 1) ])

    countby (item 0) [ "ab"; "ac"; "bc" ]
    |> should equal (Map [ ('a', 2); ('b', 1) ])

[<Fact>]
let ``test_partitionby`` () =
    let is_space (c: char) = c = ' '

    partitionby is_space "I have space"
    |> toList
    |> should
        equal
        [ [ 'I' ]
          [ ' ' ]
          [ 'h'; 'a'; 'v'; 'e' ]
          [ ' ' ]
          [ 's'; 'p'; 'a'; 'c'; 'e' ] ]

    let is_large x = x > 10

    partitionby is_large [ 1; 2; 1; 99; 88; 33; 99; -1; 5 ]
    |> toList
    |> should equal [ [ 1; 2; 1 ]; [ 99; 88; 33; 99 ]; [ -1; 5 ] ]

    partitionby id ([]: int list) |> toList |> should equal ([]: int list list)

    let vowels = "aeiou"

    partitionby (fun (c: char) -> vowels.Contains(c)) "abcdefghi"
    |> toList
    |> should equal [ [ 'a' ]; [ 'b'; 'c'; 'd' ]; [ 'e' ]; [ 'f'; 'g'; 'h' ]; [ 'i' ] ]

    partitionby id [ 1; 1; 1; 2; 3; 3; 2; 2; 3 ]
    |> map Toolz.Itertoolz.first
    |> toList
    |> should equal [ 1; 2; 3; 2; 3 ]

    partitionby id "Khhhaaaaannnnn!!!!"
    |> map Toolz.Itertoolz.first
    |> String.Concat
    |> should equal "Khan!"
