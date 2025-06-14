namespace Ramda

open FSharpPlus

[<AutoOpen>]
module Object =

    let inline assoc (prop: 'Key when 'Key: equality) (``val``: 'Value) (obj: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        obj |> Map.add prop ``val``

    let inline dissoc (prop: 'Key when 'Key: equality) (obj: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        obj |> Map.remove prop

    let inline eqProps (prop: 'Key when 'Key: equality) (obj1: Map<'Key, 'Value>) (obj2: Map<'Key, 'Value>) : bool =
        equals (obj1 |> item prop) (obj2 |> item prop)

    let inline forEachObjIndexed
        (fn: 'Value -> 'Key -> Map<'Key, 'Value> -> unit)
        (obj: Map<'Key, 'Value>)
        : Map<'Key, 'Value> when 'Key: equality =
        obj |> Map.iter (fun k v -> fn v k obj)
        obj

    let inline has (prop: 'Key when 'Key: equality) (obj: Map<'Key, 'Value>) : bool = obj |> Map.containsKey prop

    let inline invert (obj: Map<'Key, 'Value>) : Map<'Value, 'Key list> =
        obj
        |> Map.toList
        |> List.map (fun (k, v) -> v, k)
        |> List.groupBy fst
        |> Map.ofList
        |> Map.mapValues (List.map snd)

    let inline invertObj (obj: Map<'Key, 'Value>) : Map<'Value, 'Key> =
        obj
        |> Map.toList
        |> List.map (fun (k, v) -> v, k)
        |> List.groupBy fst
        |> Map.ofList
        |> Map.mapValues (List.map snd >> List.last)

    let inline keys (obj: Map<'Key, 'Value>) : 'Key seq = obj.Keys

    let inline mapObjIndexed
        (fn: 'Value -> 'Key -> Map<'Key, 'Value> -> 'Result)
        (obj: Map<'Key, 'Value>)
        : Map<'Key, 'Result> =
        obj |> Map.map (fun k v -> fn v k obj)

    let inline mergeLeft (l: Map<'Key, 'Value>) (r: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        l |> Map.fold (fun acc k v -> Map.add k v acc) r

    let inline mergeRight (l: Map<'Key, 'Value>) (r: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        r |> Map.fold (fun acc k v -> Map.add k v acc) l

    let inline mergeWith
        (fn: 'Value -> 'Value -> 'Value)
        (l: Map<'Key, 'Value>)
        (r: Map<'Key, 'Value>)
        : Map<'Key, 'Value> =
        r
        |> Map.fold
            (fun acc k v ->
                match l |> tryItem k with
                | None -> acc |> Map.add k v
                | Some lv -> Map.add k (fn lv v) acc)
            l

    let inline mergeWithKey
        (fn: 'Key -> 'Value -> 'Value -> 'Value)
        (l: Map<'Key, 'Value>)
        (r: Map<'Key, 'Value>)
        : Map<'Key, 'Value> =
        r
        |> Map.fold
            (fun acc k v ->
                match l |> tryItem k with
                | None -> acc |> Map.add k v
                | Some lv -> Map.add k (fn k lv v) acc)
            l

    let inline modify
        (prop: 'Key when 'Key: equality)
        (fn: 'Value -> 'Value)
        (obj: Map<'Key, 'Value>)
        : Map<'Key, 'Value> =
        obj |> Map.add prop (fn (item prop obj))

    let inline objOf (key: 'Key when 'Key: equality) (value: 'Value) : Map<'Key, 'Value> = Map [ key, value ]

    let inline omit (names: '``Collection<'Key>``) (obj: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        names |> fold (fun acc k -> acc |> Map.remove k) obj

    let inline pick (names: '``Collection<'Key>``) (obj: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        names
        |> fold
            (fun acc k ->
                match obj |> tryItem k with
                | None -> acc
                | Some v -> acc |> Map.add k v)
            Map.empty

    let inline pickAll (names: '``Collection<'Key>``) (obj: Map<'Key, 'Value>) : Map<'Key, 'Value option> =
        names |> fold (fun acc k -> acc |> Map.add k (obj |> tryItem k)) Map.empty

    let inline pickBy (pred: 'Value -> 'Key -> bool) (obj: Map<'Key, 'Value>) : Map<'Key, 'Value> =
        obj |> Map.filter (flip pred)

    let inline project (props: '``Collection<'Key>``) objs : '``Collection<Map<'Key, 'Value>>`` =
        objs |> map (pick props)

    let inline prop p obj = tryItem p obj

    let inline propOr ``val`` p obj =
        match obj |> tryItem p with
        | None -> ``val``
        | Some v -> v

    let inline props ps (obj: Map<'Key, 'Value>) : '``Collection<'Value>`` =
        ps |> map (flip tryItem obj >> Option.defaultValue Unchecked.defaultof<'Value>)

    let inline toPairs (obj: Map<'Key, 'Value>) : ('Key * 'Value) list = obj |> Map.toList

    let inline unwind (key: 'Key when 'Key: equality) (object: Map<'Key, 'Value>) : Map<'Key, 'Value> seq =
        match Map.tryFind key object with
        | Some values -> values |> unbox |> Seq.map (fun v -> object |> Map.add key v)
        | None -> empty

    let inline values (obj: Map<'Key, 'Value>) : 'Value seq = obj.Values

    let inline where (spec: Map<'Key, 'Value -> bool>) (testObj: Map<'Key, 'Value>) : bool =
        spec
        |> Map.forall (fun k f -> tryItem k testObj |> Option.map f |> Option.defaultValue false)

    let inline whereAny (spec: Map<'Key, 'Value -> bool>) (testObj: Map<'Key, 'Value>) : bool =
        spec
        |> Map.exists (fun k f -> tryItem k testObj |> Option.map f |> Option.defaultValue false)

    let inline whereEq (spec: Map<'Key, 'Value>) (testObj: Map<'Key, 'Value>) : bool =
        spec |> Map.mapValues equals |> flip where testObj
