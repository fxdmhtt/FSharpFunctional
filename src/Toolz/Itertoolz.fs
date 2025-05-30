namespace Toolz
open FSharpPlus

module Itertoolz =

    type GenericTail = Func with
        static member Tail (x: 'T list) = List.tail x
        static member Tail (x: 'T array) = Array.tail x
        static member Tail (x: 'T seq) = Seq.tail x

    let inline tail x = 
        let inline tail (func: ^f) (x: ^C) : 'U = 
            ((^f or ^C) : (static member Tail : ^C -> 'U) x)
        tail Func x

    let inline accumulate (binop: 'a -> 'a -> 'a) seq (initial: 'a option) =
        match initial with
        | Some init -> scan binop init seq
        | None ->
            match tryHead seq with
            | Some head -> scan binop head (tail seq)
            | None -> empty
        