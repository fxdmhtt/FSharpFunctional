namespace Ramda

open FSharpPlus

[<AutoOpen>]
module Math =

    let inline add x1 x2 = x1 + x2

    let inline dec x = x - one

    let inline divide x1 x2 = div x1 x2

    let inline inc x = x + one

    let inline mathMod x1 x2 =
        let y = x1 % x2
        if y < 0 then y + abs x2 else y

    let inline mean (xs: 'T seq) : double =
        if Seq.isEmpty xs then
            nan
        else
            let sum = Seq.sum xs
            let count = Seq.length xs
            div (double sum) (double count)

    let inline median (xs: 'T seq) : double =
        if Seq.isEmpty xs then
            nan
        else
            let sorted = xs |> Seq.sort |> Seq.toArray
            let n = Array.length sorted

            if n % 2 = 1 then
                double sorted.[n / 2]
            else
                let mid1 = sorted.[n / 2 - 1]
                let mid2 = sorted.[n / 2]
                (double mid1 + double mid2) / 2.0

    let inline modulo x1 x2 = x1 % x2

    let inline multiply x1 x2 = x1 * x2

    let inline negate x = -x

    let inline product (xs: 'T seq) : 'T = Seq.fold (*) one xs

    let inline subtract x1 x2 = x1 - x2

    let inline sum (xs: 'T seq) : 'T = Seq.fold (+) Unchecked.defaultof<'T> xs
