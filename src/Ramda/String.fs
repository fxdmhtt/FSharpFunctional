namespace Ramda
open FSharpPlus

[<AutoOpen>]
module String =
    let hello name =
        printfn "Hello %s" name
