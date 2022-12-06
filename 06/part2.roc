app "advent-06-2"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

main : Task {} []
main =
    task =
        path = Path.fromStr "input"
        contents <- await (File.readUtf8 path)
        process contents
        |> Stdout.line

    Task.attempt task \result ->
        Stdout.line (when result is
            Ok _ -> "Successfully did stuff"
            Err err -> when err is
                FileReadErr _ _ -> "Error reading file"
                _ -> "Unknown error"
        )

fragLength = 14

process : Str -> Str
process = \content ->
    content
    |> Str.graphemes
    |> \g -> List.findFirstIndex (List.range 0 (List.len g)) (\e -> List.sublist g {start: e, len: fragLength} |> is4Distinct)
    |> Result.map \i -> i+fragLength
    |> Result.withDefault 0
    |> Num.toStr

is4Distinct : List Str -> Bool
is4Distinct = \l -> l |> Set.fromList |> Set.len |> \i -> i == fragLength
