namespace Ramda
open FSharpPlus

[<AutoOpen>]
module Function =
    let hello name =
        printfn "Hello %s" name
