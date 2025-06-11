namespace Ramda

open FSharpPlus

[<AutoOpen>]
module Math =

    let inline add a b = a + b

    let inline dec n = n - one

    let inline divide a b = div a b

    let inline inc n = n + one

    let inline mathMod m p =
        let y = m % p
        if y < 0 then y + abs p else y

    let inline mean (list: 'T seq) : double =
        if Seq.isEmpty list then
            nan
        else
            let sum = Seq.sum list
            let count = Seq.length list
            div (double sum) (double count)

    let inline median (list: 'T seq) : double =
        if Seq.isEmpty list then
            nan
        else
            let sorted = list |> Seq.sort |> Seq.toArray
            let n = Array.length sorted

            if n % 2 = 1 then
                double sorted.[n / 2]
            else
                let mid1 = sorted.[n / 2 - 1]
                let mid2 = sorted.[n / 2]
                (double mid1 + double mid2) / 2.0

    let inline modulo a b = a % b

    let inline multiply a b = a * b

    let inline negate n = -n

    let inline product (list: 'T seq) : 'T = Seq.fold (*) one list

    let inline subtract a b = a - b

    let inline sum (list: 'T seq) : 'T = Seq.fold (+) Unchecked.defaultof<'T> list
