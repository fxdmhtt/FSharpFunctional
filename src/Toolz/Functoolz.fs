namespace Toolz

module Functoolz =

    let inline complement func =
        not << func

    let inline flip func a b =
        func b a

    let inline identity x =
        x
