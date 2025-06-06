namespace Toolz
open FSharpPlus

[<AutoOpen>]
module Functools =

    let inline reduce (function': 'State -> 'T -> 'State) (iterable: '``Collection<'T>``) (initial: 'State option) : 'State =
        match initial with
        | Some init -> fold function' init iterable
        | None ->
            match tryHead iterable with
            | Some head -> fold function' head (skip 1 iterable)
            | None -> invalidArg "iterable" "Cannot reduce an empty iterable without an initial value."
