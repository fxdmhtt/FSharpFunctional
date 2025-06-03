namespace Toolz
open FSharpPlus

[<AutoOpen>]
module Itertoolz =

    let inline accumulate (binop: ^a -> ^a -> ^a) seq (initial: ^a option) =
        match initial with
        | Some init -> scan binop init seq
        | None ->
            match tryHead seq with
            | Some head -> scan binop head (skip 1 seq)
            | None -> empty

    let inline concat seqs =
        seqs |> sum

    let inline cons el seq =
        result el ++ seq

    let inline count seq =
        length seq

    let inline diff seqs =
        seqs
        |> transpose
        |> fun xs ->
            xs
            // |> map (map key)
            |> map (Seq.countBy id)
            |> map (length >> (<>) 1)
            |> zip xs
        |> filter snd
        |> map fst

    let inline drop n seq =
        drop n seq

    let inline first seq =
        head seq

    let inline frequencies seq =
        seq |> Seq.countBy id |> Map.ofSeq

    let inline get ind seq =
        item ind seq

    let inline groupby key seq =
        seq
        |> groupBy key
        |> Map.ofSeq

    let inline interleave seqs =
        seqs
        |> transpose
        |> sum

    let inline interpose (el: ^a) (seq: ^a seq) : ^a seq =
        seq
        |> map (fun (x: ^a) -> Operators.seq {el; x})
        |> sum
        |> drop 1

    let inline isdistinct seq =
        seq
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.forall ((=) 1)

    let inline iterate func x =
        seq {
            let mutable current = x
            while true do
                yield current
                current <- func current
        }

    let inline join leftkey leftseq rightkey rightseq =
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

    let inline last seq =
        seq |> rev |> head

    let inline mapcat func seqs =
        seqs
        |> map func
        |> concat

    let inline nth n seq =
        seq |> skip n |> head

    let inline partition n seq =
        seq
        |> Seq.chunkBySize n
        |> map Seq.toList
        |> filter (length >> (=) n)

    let inline partition_all n seq =
        seq
        |> Seq.chunkBySize n
        |> map Seq.toList

    let inline peek seq =
        head seq, seq

    let inline peekn n seq =
        take n seq, seq

    let inline pluck ind seqs =
        seqs
        |> map (item ind)

    let inline random_sample prob seq =
        seq
        |> filter (fun _ -> System.Random().NextDouble() < prob)

    let inline reduceby key (binop: ^a -> ^a -> ^a) seq (init: ^a option) =
        seq
        |> groupby key
        |> Map.map (fun _ group -> Functools.reduce binop group init)

    let inline remove predicate seq =
        seq
        |> filter (not << predicate)

    let inline second seq =
        seq |> drop 1 |> head

    let inline sliding_window n seq =
        seq
        |> Seq.windowed n

    let inline tail n seq =
        seq
        |> skip (length seq - n)

    let inline take_nth n seq =
        seq
        |> mapi (fun i x -> x, i % n = 0)
        |> filter snd
        |> map fst

    let inline topk k seq key =
        seq
        |> sortByDescending key
        |> take k

    let inline unique seq key =
        seq
        |> groupBy key
        |> map snd
        |> map head
