namespace Ramda

open FSharpPlus

[<AutoOpen>]
module Logic =

    let inline ``and`` (a: bool) (b: bool) : bool = a && b

    let inline complement f = not << f

    let inline cond pairs =
        let rec loop pairs x =
            match pairs with
            | [] -> failwith "No matching condition"
            | (predicate, transformer) :: rest -> if predicate x then transformer x else loop rest x

        loop pairs

    let inline ``not`` (a: bool) : bool = not a

    let inline ``or`` (a: bool) (b: bool) : bool = a || b

    let inline ``xor`` (a: bool) (b: bool) : bool = a <> b
