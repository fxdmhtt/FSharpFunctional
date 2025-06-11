namespace Ramda

open FSharpPlus

[<AutoOpen>]
module Function =

    let inline always (``val``: 'T) : unit -> 'T = fun _ -> ``val``

    let inline ap applyF applyX = applyF <*> applyX

    let inline applyTo x f = f x

    let inline ascend (fn: 'T -> _) : 'T -> 'T -> int = fun x y -> compare (fn x) (fn y)

    let inline comparator (pred: 'T -> 'T -> bool) : 'T -> 'T -> int =
        fun x y ->
            if pred x y then -1
            else if pred y x then 1
            else 0

    let inline descend (fn: 'T -> _) : 'T -> 'T -> int = fun x y -> compare (fn y) (fn x)

    let inline F () : bool = false

    let inline identity (x: 'T) : 'T = x

    let inline ``of`` (x: 'T) : 'T list = [ x ]

    let inline on (f: 'U -> 'U -> 'V) (g: 'T -> 'U) (a: 'T) (b: 'T) : 'V = f (g a) (g b)

    let inline once fn =
        let mutable called = false
        let mutable result = Unchecked.defaultof<'T>

        fun x ->
            if not called then
                called <- true
                result <- fn x

            result

    let inline T () : bool = true
