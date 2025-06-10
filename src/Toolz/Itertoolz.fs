namespace Toolz
open FSharpPlus
open FSharpPlus.Data

[<AutoOpen>]
module Itertoolz =

    let inline accumulate (binop: 'State -> 'T -> 'State) (seq: '``Collection<'T>``) (initial: 'State option) : '``Collection<'State>`` =
        match initial with
        | Some init -> scan binop init seq
        | None ->
            match tryHead seq with
            | Some head -> scan binop head (skip 1 seq)
            | None -> empty

    let inline concat (seqs: '``Collection<'Collection<'T>>``) : '``Collection<'T>`` =
        seqs |> sum

    let inline cons (el: 'T) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
        result el ++ seq

    let inline count (seq: '``Collection<'T>``) : int =
        length seq

    let inline diff (seqs: '``Collection<'Collection<'T>>``) (key: 'T -> 'Key when 'Key : equality) : '``Collection<'T>`` =
        seqs
        |> transpose
        |> fun xs ->
            xs
            |> map (map key)
            |> map (Seq.countBy id)
            |> map (length >> (<>) 1)
            |> zip xs
        |> filter snd
        |> map fst

    let inline drop (n: int) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
        drop n seq

    let inline first (seq: '``Collection<'T>``) : 'T =
        head seq

    let inline frequencies (seq: '``Collection<'T>``) : Map<'T, int> =
        seq |> Seq.countBy id |> Map.ofSeq

    let inline get (ind: 'Index) (seq: '``Collection<'T>``) : 'T =
        item ind seq

    // let inline groupby (key: 'T -> 'Key when 'Key : equality) (seq: '``Collection<'T>``) : Map<'Key, '``Collection<'T>``> =
    let inline groupby (key: 'T -> 'Key when 'Key : equality) (seq: '``Collection<'T>``) =
        seq
        |> groupBy key
        |> Map.ofSeq

    let inline interleave (seqs: '``Collection<'Collection<'T>>``) : '``Collection<'T>`` =
        seqs
        |> transpose
        |> sum

    // let inline interpose (el: 'T) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
    let inline interpose (el: 'T) seq =
        seq |> intersperse el

    let inline isdistinct (seq: '``Collection<'T>``) : bool =
        seq
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.forall ((=) 1)

    let inline iterate (func: 'T -> 'T) (x: 'T) : 'T seq =
        seq {
            let mutable current = x
            while true do
                yield current
                current <- func current
        }

    let inline join (leftkey: 'T -> 'Key when 'Key : equality) (leftseq: '``Collection<'T>``) (rightkey: 'U -> 'Key when 'Key : equality) (rightseq: '``Collection<'U>``) : ('T * 'U) seq =
        let d = groupby leftkey leftseq

        seq {
            for item in rightseq do
                let key = rightkey item
                match Map.tryFind key d with
                | Some left_matchs ->
                    for left_match in left_matchs do
                        yield (left_match, item)
                | None -> ()
        }

    let inline last (seq: '``Collection<'T>``) : 'T =
        seq |> rev |> head

    // let inline mapcat (func: '``Collection<'T>`` -> '``Collection<'U>``) (seqs: '``Collection<'Collection<'T>>``) : '``Collection<'U>`` =
    let inline mapcat (func: '``Collection<'T>`` -> '``Collection<'U>``) seqs : '``Collection<'U>`` =
        seqs
        |> map func
        |> concat

    let inline nth (n: int) (seq: '``Collection<'T>``) : 'T =
        seq |> skip n |> head

    let inline partition (n: int) (seq: '``Collection<'T>``) : 'T list seq =
        seq
        |> Seq.chunkBySize n
        |> map Seq.toList
        |> filter (length >> (=) n)

    let inline partition_all (n: int) (seq: '``Collection<'T>``) : 'T list seq =
        seq
        |> Seq.chunkBySize n
        |> map Seq.toList

    let inline peek (seq: '``Collection<'T>``) : 'T * '``Collection<'T>`` =
        head seq, seq

    let inline peekn (n: int) (seq: '``Collection<'T>``) : '``Collection<'T>`` * '``Collection<'T>`` =
        take n seq, seq

    // let inline pluck (ind: 'Index) (seqs: '``Collection<'Indexed<'T>>``) : '``Collection<'T>`` =
    let inline pluck (ind: 'Index) seqs : '``Collection<'T>`` =
        seqs
        |> map (item ind)

    let inline random_sample (prob: float) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
        seq
        |> filter (fun _ -> System.Random().NextDouble() < prob)

    // let inline reduceby (key: 'T -> 'Key when 'Key : equality) (binop: 'State -> 'T -> 'State) (seq: '``Collection<'T>``) (init: 'State option) : Map<'Key, 'State> =
    let inline reduceby key binop seq (init: 'State option) : Map<'Key, 'State> =
        seq
        |> groupby key
        |> Map.mapValues (fun group ->
            match init with
            | Some initial -> fold binop initial seq
            | None ->
                group
                |> NonEmptySeq.tryOfSeq
                |> Option.map (NonEmptySeq.reduce binop)
                |> Option.defaultValue Unchecked.defaultof<'State>
        )

    let inline remove (predicate: 'T -> bool) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
        seq
        |> filter (not << predicate)

    let inline second (seq: '``Collection<'T>``) : 'T =
        seq |> drop 1 |> head

    let inline sliding_window (n: int) (seq: '``Collection<'T>``) : 'T array seq =
        seq
        |> Seq.windowed n

    let inline tail (n: int) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
        seq
        |> skip (length seq - n)

    // let inline take_nth (n: int) (seq: '``Collection<'T>``) : '``Collection<'T>`` =
    let inline take_nth (n: int) seq : '``Collection<'T>`` =
        seq
        |> mapi (fun i x -> x, i % n = 0)
        |> filter snd
        |> map fst

    let inline topk (k: int) (seq: '``Collection<'T>``) (key: 'T -> 'Key when 'Key : equality) : '``Collection<'T>`` =
        seq
        |> sortByDescending key
        |> take k

    let inline unique (seq: '``Collection<'T>``) (key: 'T -> 'Key when 'Key : equality) : '``Collection<'T>`` =
        seq
        |> groupBy key
        |> map snd
        |> map head
