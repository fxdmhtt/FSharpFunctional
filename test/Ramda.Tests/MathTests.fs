module MathTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Ramda

[<Fact>]
let ``test_add`` () =
    add 2 3 |> should equal 5
    add 7 10 |> should equal 17

    add 0 0 |> should equal 0
    add -1 1 |> should equal 0
    add -5 -5 |> should equal -10
    add 1.5 2.5 |> should equal 4.0

[<Fact>]
let ``test_dec`` () =
    dec 42 |> should equal 41

    dec 2 |> should equal 1
    dec 7 |> should equal 6
    dec 0 |> should equal -1
    dec -1 |> should equal -2
    dec -5 |> should equal -6
    dec 3.5 |> should equal 2.5

[<Fact>]
let ``test_divide`` () =
    divide 71.0 100 |> should equal 0.71

    let half = flip divide 2
    half 42 |> should equal 21

    let reciprocal = divide 1.0
    reciprocal 4 |> should equal 0.25

    divide 10 2 |> should equal 5
    divide 9 3 |> should equal 3
    divide 0 1 |> should equal 0
    divide -6 -2 |> should equal 3
    divide -10 2 |> should equal -5
    divide 7.5 2.5 |> should equal 3.0

[<Fact>]
let ``test_inc`` () =
    inc 42 |> should equal 43

    inc 2 |> should equal 3
    inc 7 |> should equal 8
    inc 0 |> should equal 1
    inc -1 |> should equal 0
    inc -5 |> should equal -4
    inc 3.5 |> should equal 4.5

[<Fact>]
let ``test_mathMod`` () =
    mathMod -17 5 |> should equal 3
    mathMod 17 5 |> should equal 2
    // mathMod 17 -5 |> should be NaN
    // mathMod 17 0 |> should be NaN
    // mathMod 17.2 5 |> should be NaN
    // mathMod 17 5.3 |> should be NaN

    let clock = flip mathMod 12
    clock 15 |> should equal 3
    clock 24 |> should equal 0

    let seventeenMod = mathMod 17
    seventeenMod 3 |> should equal 2
    seventeenMod 4 |> should equal 1
    seventeenMod 10 |> should equal 7

[<Fact>]
let ``test_mean`` () =
    mean [ 2; 7; 9 ] |> should equal 6.0
    mean [] |> should be NaN

    mean [ 1; 2; 3; 4; 5 ] |> should equal 3.0
    mean [ 10; 20; 30 ] |> should equal 20.0
    mean [] |> should be NaN
    mean [ 1.5; 2.5; 3.5 ] |> should equal 2.5
    mean [ -1; -2; -3 ] |> should equal -2.0

[<Fact>]
let ``test_median`` () =
    median [ 2; 9; 7 ] |> should equal 7.0
    median [ 7; 2; 10; 9 ] |> should equal 8.0
    median [] |> should be NaN

    median [ 1; 2; 3; 4; 5 ] |> should equal 3.0
    median [ 1; 2; 3; 4 ] |> should equal 2.5

    median [ 10; 20; 30 ] |> should equal 20.0
    median [ 1.5; 2.5; 3.5 ] |> should equal 2.5
    median [ -1; -2; -3 ] |> should equal -2.0
    median [ 7; 8; 9; 10; 11 ] |> should equal 9.0

[<Fact>]
let ``test_modulo`` () =
    modulo 17 3 |> should equal 2
    modulo -17 3 |> should equal -2
    modulo 17 -3 |> should equal 2

    let isOdd = flip modulo 2
    isOdd 42 |> should equal 0
    isOdd 21 |> should equal 1

[<Fact>]
let ``test_multiply`` () =
    let double = multiply 2
    let triple = multiply 3
    double 3 |> should equal 6
    triple 4 |> should equal 12
    multiply 2 5 |> should equal 10

    multiply 2 3 |> should equal 6
    multiply 7 10 |> should equal 70

    multiply 0 5 |> should equal 0
    multiply -1 1 |> should equal -1
    multiply -5 -5 |> should equal 25
    multiply 1.5 2.5 |> should equal 3.75

    let double = flip multiply 2
    double 3 |> should equal 6
    double -4 |> should equal -8
    let double = flip multiply 2.0
    double 0.5 |> should equal 1.0

[<Fact>]
let ``test_negate`` () =
    negate 42 |> should equal -42

    negate -1 |> should equal 1
    negate 0 |> should equal 0
    negate 3.5 |> should equal -3.5

[<Fact>]
let ``test_product`` () =
    product [ 2; 4; 6; 8; 100; 1 ] |> should equal 38400

    product [ 2; 3; 4 ] |> should equal 24
    product [] |> should equal 1

    product [ 1; 2; 3; 4; 5 ] |> should equal 120
    product [ 10; 20; 30 ] |> should equal 6000
    product [] |> should equal 1
    product [ 1.5; 2.5; 3.5 ] |> should equal 13.125
    product [ -1; -2; -3 ] |> should equal -6

[<Fact>]
let ``test_subtract`` () =
    subtract 10 8 |> should equal 2

    let minus5 = flip subtract 5
    minus5 17 |> should equal 12

    let complementaryAngle = subtract 90
    complementaryAngle 30 |> should equal 60
    complementaryAngle 72 |> should equal 18

    subtract 5 3 |> should equal 2
    subtract 10 7 |> should equal 3

    subtract 0 0 |> should equal 0
    subtract -1 1 |> should equal -2
    subtract -5 -5 |> should equal 0
    subtract 1.5 2.5 |> should equal -1.0

    let minusTwo = flip subtract 2
    minusTwo 5 |> should equal 3
    minusTwo -1 |> should equal -3
    minusTwo 0 |> should equal -2

[<Fact>]
let ``test_sum`` () =
    sum [ 2; 4; 6; 8; 100; 1 ] |> should equal 121

    sum [ 1; 2; 3 ] |> should equal 6
    sum [] |> should equal 0

    sum [ 1; 2; 3; 4; 5 ] |> should equal 15
    sum [ 10; 20; 30 ] |> should equal 60
    sum [] |> should equal 0
    sum [ 1.5; 2.5; 3.5 ] |> should equal 7.5
    sum [ -1; -2; -3 ] |> should equal -6
