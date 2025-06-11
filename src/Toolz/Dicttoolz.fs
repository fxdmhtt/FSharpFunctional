namespace Toolz

open FSharpPlus

[<AutoOpen>]
module Dicttoolz =

    let inline assoc (d: Map<'Key, 'Value>) (key: 'Key) (value: 'Value) : Map<'Key, 'Value> = d |> Map.add key value

    let inline dissoc (d: Map<'Key, 'Value>) (keys: '``Collection<'Key>``) : Map<'Key, 'Value> =
        keys |> fold (flip Map.remove) d

    let inline itemfilter (predicate: 'Key * 'Value -> bool) (d: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        d |> Map.filter (curry predicate)

    let inline itemmap
        (func: 'TKey1 * 'TValue1 -> 'TKey2 * 'TValue2)
        (d: Map<'TKey1, 'TValue1>)
        : Map<'TKey2, 'TValue2> =
        d |> Map.map (curry func) |> Map.values |> Map.ofSeq

    let inline keyfilter (predicate: 'Key -> bool) (d: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        d |> Map.filter (curry (item1 >> predicate))

    let inline keymap (func: 'T1 -> 'T2) (d: Map<'T1, 'Value>) : Map<'T2, 'Value> =
        d |> Map.map (curry (Tuple2.mapItem1 func)) |> Map.values |> Map.ofSeq

    let inline merge (dicts: '``Collection<Map<'Key, 'Value>>``) : Map<'Key, 'Value> =
        dicts |> rev |> fold Map.union Map.empty

    // let inline merge_with (func: 'Value list -> 'TValue) (dicts: '``Collection<Map<'Key, 'Value>>``) : Map<'Key, 'TValue> =
    let inline merge_with (func: 'Value list -> 'TValue) dicts : Map<'Key, 'TValue> =
        dicts
        |> map (Map.mapValues result)
        |> fold (Map.unionWith (@)) Map.empty
        |> Map.mapValues func

    let inline valfilter (predicate: 'Value -> bool) (d: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        d |> Map.filter (curry (item2 >> predicate))

    let inline valmap (func: 'T1 -> 'T2) (d: Map<'Key, 'T1>) : Map<'Key, 'T2> = d |> Map.mapValues func
