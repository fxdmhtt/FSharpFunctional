namespace Ramda
open FSharpPlus

[<AutoOpen>]
module Type =
    let hello name =
        printfn "Hello %s" name
