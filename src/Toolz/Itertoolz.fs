namespace Toolz
open FSharpPlus

module Itertoolz =

    let inline accumulate (binop: 'a -> 'a -> 'a) seq (initial: 'a option) =
        match initial with
        | Some init -> scan binop init seq
        | None ->
            match tryHead seq with
            | Some head -> scan binop head (skip 1 seq)
            | None -> empty
        