namespace Ramda

open FSharpPlus

[<AutoOpen>]
module String =

    let inline ``match`` (rx: string) (str: string) : string list =
        System.Text.RegularExpressions.Regex.Matches(str, rx)
        |> Seq.cast<System.Text.RegularExpressions.Match>
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList

    let inline replace (pattern: string) (replacement: string) (str: string) : string =
        System.Text.RegularExpressions.Regex.Replace(str, pattern, replacement)

    let inline split (sep: string) (str: string) : string list = str.Split(sep) |> Seq.toList

    let inline test (pattern: string) (str: string) : bool =
        System.Text.RegularExpressions.Regex.IsMatch(str, pattern)

    let inline toLower (str: string) : string = str.ToLowerInvariant()

    let inline toString (``val``: obj) : string = ``val``.ToString()

    let inline toUpper (str: string) : string = str.ToUpperInvariant()

    let inline trim (str: string) : string = str.Trim()
