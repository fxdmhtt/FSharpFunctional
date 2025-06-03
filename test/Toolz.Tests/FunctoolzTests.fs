module FunctoolzTests

open System
open Xunit
open FsUnit.Xunit
open FsCheck.Xunit
open Toolz.Functoolz

let iseven x = x % 2 = 0
let isodd x = x % 2 = 1
let inc x = x + 1
let double x = 2 * x

[<Fact>]
let ``test_complement`` () =
    // No args:
    complement (fun () -> false) () |> should be True
    complement (fun () -> true) () |> should be False

    // Single arity:
    complement iseven 1 |> should be True
    complement iseven 2 |> should be False
    complement (complement iseven) 2 |> should be True
    complement (complement isodd) 2 |> should be False

    // Multiple arities:
    // both_even = lambda a, b: iseven(a) and iseven(b)
    // assert complement(both_even)(1, 2)
    // assert not complement(both_even)(2, 2)

    // Generic truthiness:
    // assert complement(lambda: "")()
    // assert complement(lambda: 0)()
    // assert complement(lambda: None)()
    // assert complement(lambda: [])()

    // assert not complement(lambda: "x")()
    // assert not complement(lambda: 1)()
    // assert not complement(lambda: [1])()

[<Fact>]
let ``test_flip`` () =
    let f a b = a, b
    
    flip f 'a' 'b' |> should equal ('b', 'a')

[<Property>]
let ``test_flip_property`` (a: float) (b: float) =
    let f a b = a, b

    if System.Double.IsNaN(a) || System.Double.IsNaN(b) then 
        true
    else
        flip f a b = (b, a)
