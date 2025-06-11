namespace Toolz

open FSharpPlus

[<AutoOpen>]
module Recipes =

    let inline countby (key: 'T -> 'Key when 'Key: equality) (seq: '``Collection<'T>``) : Map<'Key, int> =
        seq |> Seq.countBy key |> Map.ofSeq

    let inline partitionby
        (func: 'T -> 'Key when 'Key: equality)
        (seq: '``Collection<'T>``)
        : '``Collection<'T list>`` =
        seq |> chunkBy func |> map (snd >> toList)
