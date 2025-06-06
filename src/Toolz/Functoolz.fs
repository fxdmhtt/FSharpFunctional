namespace Toolz

module Functoolz =

    let inline complement (func: 'T -> bool) : 'T -> bool =
        not << func

    let inline identity (x: 'T) : 'T =
        x
