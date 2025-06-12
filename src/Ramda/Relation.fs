namespace Ramda

open FSharpPlus

[<AutoOpen>]
module Relation =

    let inline clamp (minimum: 'T) (maximum: 'T) (value: 'T) : 'T = min (max minimum value) maximum

    let inline countBy (fn: 'T -> 'Key) (list: '``Collection<'T>``) : Map<'Key, int> =
        list |> groupBy fn |> Map.ofSeq |> Map.mapValues length

    let inline difference (list1: '``Collection<'T>``) (list2: '``Collection<'T>``) : '``Collection<'T>`` =
        let set2 = Set.ofSeq list2
        list1 |> filter (not << set2.Contains)

    let inline differenceWith (pred: 'T -> 'T -> bool) list1 list2 : '``Collection<'T>`` =
        let lst2 = list2 |> toList
        list1 |> filter (fun x -> lst2 |> List.forall (not << pred x))

    let inline eqBy f (x: 'T) (y: 'T) : bool = f x = f y

    let inline equals (a: 'T) (b: 'T) : bool = a = b

    let inline gt (a: 'T) (b: 'T) : bool = a > b

    let inline gte (a: 'T) (b: 'T) : bool = a >= b

    let inline innerJoin
        (pred: 'T -> 'U -> bool)
        (xs: '``Collection<'T>``)
        (ys: '``Collection<'U>``)
        : '``Collection<'T>`` =
        let lst2 = ys |> toSeq |> Seq.toList
        xs |> filter (fun x -> lst2 |> exists (pred x))

    let inline intersection (list1: '``Collection<'T>``) (list2: '``Collection<'T>``) : '``Collection<'T>`` =
        let lst1 = list1 |> toSeq |> Set.ofSeq
        list2 |> distinct |> filter lst1.Contains

    let inline lt (a: 'T) (b: 'T) : bool = a < b

    let inline lte (a: 'T) (b: 'T) : bool = a <= b

    let inline max (a: 'T) (b: 'T) : 'T = if a > b then a else b

    let inline maxBy (f: 'T -> _) (a: 'T) (b: 'T) : 'T =
        let a' = f a
        let b' = f b
        if a' > b' then a else b

    let inline min (a: 'T) (b: 'T) : 'T = if a < b then a else b

    let inline minBy (f: 'T -> _) (a: 'T) (b: 'T) : 'T =
        let a' = f a
        let b' = f b
        if a' < b' then a else b

    let inline sortWith (functions: list<'T -> 'T -> int>) (list: '``Collection<'T>``) : '``Collection<'T>`` =
        let rec loop fs a b =
            match fs with
            | [] -> 0
            | f :: rest ->
                match f a b with
                | 0 -> loop rest a b
                | res -> res

        let comp = loop functions

        list |> List.sortWith comp

    let inline symmetricDifference (list1: '``Collection<'T>``) (list2: '``Collection<'T>``) : '``Collection<'T>`` =
        let inters =
            Set.intersect (list1 |> toSeq |> Set.ofSeq) (list2 |> toSeq |> Set.ofSeq)

        list1 ++ list2 |> filter (not << inters.Contains)

    let inline symmetricDifferenceWith (pred: 'T -> 'T -> bool) list1 list2 : '``Collection<'T>`` =
        let lst1 = list1 |> toList
        let lst2 = list2 |> toList
        differenceWith pred lst1 lst2 ++ differenceWith pred lst2 lst1

    let inline union (``as``: '``Collection<'T>``) (bs: '``Collection<'T>``) : '``Collection<'T>`` =
        ``as`` ++ bs |> distinct

    let inline unionWith (pred: 'T -> 'T -> bool) (list1: '``Collection<'T>``) (list2: '``Collection<'T>``) : 'T list =
        list1 ++ list2
        |> fold (fun acc x -> if acc |> List.forall (not << pred x) then x :: acc else acc) []
        |> rev
