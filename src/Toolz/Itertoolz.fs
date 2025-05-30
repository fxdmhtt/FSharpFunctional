namespace Toolz
open FSharpPlus

module Itertoolz =

    type Tail =
        static member Tail (x: 'T list) = List.tail x
        static member Tail (x: 'T array) = Array.tail x
        static member Tail (x: 'T seq) = Seq.tail x

        static member inline Invoke source =
            let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Tail : _ -> _) b)
            let inline call (a: 'a, b: 'b) = call_2 (a, b)
            call (Unchecked.defaultof<Tail>, source)

    let inline accumulate (binop: 'a -> 'a -> 'a) seq (initial: 'a option) =
        let inline tail source = Tail.Invoke source

        match initial with
        | Some init -> scan binop init seq
        | None ->
            match tryHead seq with
            | Some head -> scan binop head (tail seq)
            | None -> empty
        