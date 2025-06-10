namespace Ramda
open FSharpPlus

[<AutoOpen>]
module Object =
    let hello name =
        printfn "Hello %s" name
